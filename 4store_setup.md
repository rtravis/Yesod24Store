## Setting Up a 4store RDF Backend

First install the "4store" package using your favourite package manager.


## Creating the Backend (Knowledge Base)

We'll call our backend 'homer' and create it using the command:

```
sudo 4s-backend-setup homer --node 0 --cluster 1 --segments 2
```

After the backend creation we must start it so that it becomes available:

`sudo 4s-backend homer`


## Importing RDF Data to Work With

Once the backend is started we can import new data and query it

```
4s-import homer -v /tmp/LinkedDataFiles/rdf.ttl
4s-import homer -v /tmp/LinkedDataFiles/rdfs.ttl
4s-import homer -M "http://myhost/AddressBook" -v /tmp/LinkedDataFiles/AddressBook.nt
```
... and so on ...


## Querying Data and Displaying Statistics

You can query statistics about your knowledge base (backend)

```
sudo 4s-backend-info homer
4s-size homer
```

or run SPARQL queries directly from the terminal:

```
4s-query -P homer < ~/Documents/myquery.rq
4s-query -f sparql homer 'SELECT * WHERE { ?s ?p ?o } LIMIT 2'
```

## Deleting Models and Destroying the Backend

To delete a named graph (model) use the command:

`4s-delete-model homer "http://myhost/AddressBook"`

or you can delete all your models at once:

`4s-delete-model homer --all`

to destroy the backend and all its data you can issue the command:

`sudo 4s-backend-destroy homer`

finaly the backend process can be shutdown:

`killall -TERM 4s-backend`
