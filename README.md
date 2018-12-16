## Yesod24Store - Haskell Web Application

This project is the result of trying to learn to use a contemporary web
development framework. The public repository was created with the intention of
helping others who want to learn to use the [Yesod web framework](https://www.yesodweb.com/).

The project consists of a very simple and clumsy web application that can submit
SPARQL queries from a web-browser to a [4store](https://4store.github.io/)
back-end. The query results are displayed in the browser.

## Requirements

- preferably a Linux based operating system
- the Glasgow Haskell Compilation system (the "ghc" package)
- the "libghc-yesod-dev" and the "4store" packages

## Running

Follow the steps from the [4store_setup.md] file to setup a 4store knowledge base
backend and populate it with data. Then, the easiest thing to do is to just run
the Haskell interpreter:

```
runhaskell RdfPortal.hs
```

and access the application from the browser at: http://localhost:3000/

## License

Yesod24Store is MIT-licensed, as found in the LICENSE file.