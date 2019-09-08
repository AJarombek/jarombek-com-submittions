#!/usr/bin/env bash

# Commands for interacting with the ElasticSearch server
# Author: Andrew Jarombek
# Date: 5/9/2019

ES_ENDPOINT=https://search-sandbox-elasticsearch-demo-krzp4p5roknptbcoow2xlb5sby.us-east-1.es.amazonaws.com

# Delete, create, and retrieve a document representing an author on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/meta/author/1
curl -XPUT ${ES_ENDPOINT}/meta/author/1 -H 'Content-Type: application/json' -d @es-documents/author.json
curl ${ES_ENDPOINT}/meta/author/1

# Add a document to the lp index and plate type
curl -XPUT ${ES_ENDPOINT}/lp/plates/1 -d @es-endpoints/ct_2010_passenger_lp.json