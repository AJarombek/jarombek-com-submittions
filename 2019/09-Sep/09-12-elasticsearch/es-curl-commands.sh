#!/usr/bin/env bash

# Commands for interacting with the ElasticSearch server
# Author: Andrew Jarombek
# Date: 5/9/2019

ES_ENDPOINT=https://search-sandbox-elasticsearch-demo-krzp4p5roknptbcoow2xlb5sby.us-east-1.es.amazonaws.com

# Delete, create, and retrieve an index for an author on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/author
curl -XPUT ${ES_ENDPOINT}/author -H 'Content-Type: application/json' -d @es-documents/author/index.json
curl ${ES_ENDPOINT}/author?pretty=true

# Delete, create, and retrieve a document representing an author on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/author/_doc/1
curl -XPUT ${ES_ENDPOINT}/author/_doc/1 -H 'Content-Type: application/json' -d @es-documents/author/andy.json
curl ${ES_ENDPOINT}/author/_doc/1
curl ${ES_ENDPOINT}/author/_doc/1?pretty=true

# Delete, create, and retrieve an index for license plates on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/license_plate
curl -XPUT ${ES_ENDPOINT}/license_plate -H 'Content-Type: application/json' -d @es-documents/license_plate/index.json
curl ${ES_ENDPOINT}/license_plate?pretty=true

# Add a document to the lp index and plate type
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/1 -d @es-endpoints/license_plate/ct_2017_passenger.json