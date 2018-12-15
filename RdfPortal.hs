{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text, unpack, pack, stripSuffix)
import           System.Process      (readProcess)
import           Text.Hamlet         (hamletFile)
import           Text.Julius         (RawJS (..))
import           Yesod

-- local imports are below this line
import           Settings


data RdfPortalApp = RdfPortalApp

mkYesod "RdfPortalApp" $(parseRoutesFile "config/routes")

instance Yesod RdfPortalApp


-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage RdfPortalApp FormMessage where
    renderMessage _ _ = defaultFormMessage


-- The datatype we wish to receive from the form
data UserInput = UserInput
    { queryText :: Textarea
    }
  deriving Show

-- Declare the form.
--
-- * We have our Handler as the inner monad, which indicates which site this is
-- running in.
--
-- * FormResult can be in three states: FormMissing (no data available),
-- FormFailure (invalid data) and FormSuccess
--
-- * The Widget is the viewable form to place into the web page.
userInputForm :: Html -> MForm Handler (FormResult UserInput, Widget)
userInputForm = renderDivs $ UserInput
    <$> areq textareaField textSettings (Just $ Textarea "")
    where textSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "textarea1"
            , fsAttrs =
                [ ("rows", "10")
                , ("cols", "65")
                , ("oninput", "inputChanged(event)")
                , ("placeholder", "input message here")
                , ("class", "whole frm__text")
                ]
            }

layoutPage :: (Widget, Enctype, Text) -> Handler Html
layoutPage (userInputWidget, enctype, queryResult) = do
    let (resultId) = getElemIds
    pc <- widgetToPageContent $ do
        setTitle "RDF Portal"
        $(widgetFile "homepage")
    withUrlRenderer $(hamletFile "templates/homepage-wrapper.hamlet")

-- The GET handler displays the form
getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (userInputWidget, enctype) <- generateFormPost userInputForm
    layoutPage (userInputWidget, enctype, "")

queryCommand :: String
queryCommand = "/usr/bin/4s-query"

knowledgeBase :: String
knowledgeBase = "homer"

formatString :: Bool -> String
formatString isJsonFormat =
    case isJsonFormat of
        True -> "json"
        _ -> "text"

commandArgs :: Bool -> [String]
commandArgs isJson = ["-P", "-f", formatString isJson, knowledgeBase]

runSparqlQuery :: Bool -> Text -> IO Text
runSparqlQuery jsonFormat input = do
    let format = "json"
    x <- readProcess queryCommand (commandArgs jsonFormat) (unpack input)
    case stripSuffix "#EOR\n" (pack x) of
        Just y -> return y
        _ -> return (pack x)

-- extract the SPARQL query from the UserInput form and run it
runSparqlQueryUI :: UserInput -> IO Text
runSparqlQueryUI (UserInput (Textarea x)) = runSparqlQuery False x

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postHomeR :: Handler Html
postHomeR = do
    ((result, userInputWidget), enctype) <- runFormPost userInputForm
    case result of
        FormSuccess userInput -> do
            result <- liftIO (runSparqlQueryUI userInput)
            layoutPage (userInputWidget, enctype, result)
        _ -> do
            layoutPage (userInputWidget, enctype, "form post failure")


data AjaxMessage = AjaxMessage { getmsg :: Text }

instance FromJSON AjaxMessage where
    parseJSON (Object o) = AjaxMessage <$> (o .: "message")
    parseJSON _ = fail $ "unexpected JSON"

postAjaxUpdateR :: Handler TypedContent
postAjaxUpdateR = do
    req <- requireJsonBody :: Handler AjaxMessage
    qresult <- liftIO $ runSparqlQuery True $ getmsg req
    return $ TypedContent "application/sparql-results+json" $ toContent qresult

getElemIds :: (Text)
getElemIds = ("js-resultId")

main :: IO ()
main = warp 3000 RdfPortalApp
