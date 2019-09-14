#!/usr/bin/env bash

# Commands for interacting with the ElasticSearch server
# Author: Andrew Jarombek
# Date: 5/9/2019

ES_ENDPOINT=https://search-sandbox-elasticsearch-demo-krzp4p5roknptbcoow2xlb5sby.us-east-1.es.amazonaws.com

# Delete, create, and retrieve an index for an author on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/author
curl -XPUT ${ES_ENDPOINT}/author -H 'Content-Type: application/json' -d @data/author/index.json
curl ${ES_ENDPOINT}/author?pretty=true

# Delete, create, and retrieve a document representing an author on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/author/_doc/1
curl -XPUT ${ES_ENDPOINT}/author/_doc/1 -H 'Content-Type: application/json' -d @data/author/andy.json
curl ${ES_ENDPOINT}/author/_doc/1
curl ${ES_ENDPOINT}/author/_doc/1?pretty=true

# Delete, create, and retrieve an index for license plates on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/license_plate
curl -XPUT ${ES_ENDPOINT}/license_plate -H 'Content-Type: application/json' -d @data/license_plate/index.json
curl ${ES_ENDPOINT}/license_plate?pretty=true

# Delete the documents in the plates index
curl -XDELETE ${ES_ENDPOINT}/license_plate/_doc/1
curl -XDELETE ${ES_ENDPOINT}/license_plate/_doc/2
curl -XDELETE ${ES_ENDPOINT}/license_plate/_doc/3
curl -XDELETE ${ES_ENDPOINT}/license_plate/_doc/4
curl -XDELETE ${ES_ENDPOINT}/license_plate/_doc/5
curl -XDELETE ${ES_ENDPOINT}/license_plate/_doc/6
curl -XDELETE ${ES_ENDPOINT}/license_plate/_doc/7

# Add documents to the plates index
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/1 -H 'Content-Type: application/json' \
    -d @data/license_plate/ct_2017_passenger.json
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/2 -H 'Content-Type: application/json' \
    -d @data/license_plate/ct_1987_passenger.json
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/3 -H 'Content-Type: application/json' \
    -d @data/license_plate/nv_2002_passenger.json
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/4 -H 'Content-Type: application/json' \
    -d @data/license_plate/qr_2014_passenger.json
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/5 -H 'Content-Type: application/json' \
    -d @data/license_plate/sk_1989_passenger.json
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/6 -H 'Content-Type: application/json' \
    -d @data/license_plate/ny_1999_passenger.json
curl -XPUT ${ES_ENDPOINT}/license_plate/_doc/7 -H 'Content-Type: application/json' \
    -d @data/license_plate/ga_1998_passenger.json

# Retrieve the documents in the plates index
curl ${ES_ENDPOINT}/license_plate/_doc/1?pretty=true
curl ${ES_ENDPOINT}/license_plate/_doc/2?pretty=true
curl ${ES_ENDPOINT}/license_plate/_doc/3?pretty=true
curl ${ES_ENDPOINT}/license_plate/_doc/4?pretty=true
curl ${ES_ENDPOINT}/license_plate/_doc/5?pretty=true
curl ${ES_ENDPOINT}/license_plate/_doc/6?pretty=true
curl ${ES_ENDPOINT}/license_plate/_doc/7?pretty=true

# Retrieve the type mappings for the license_plate and author indexes.
curl ${ES_ENDPOINT}/author/_mapping?pretty=true
curl ${ES_ENDPOINT}/license_plate/_mapping?pretty=true

# Search all indexes.
curl ${ES_ENDPOINT}/_search?pretty=true

# Search the entire license_plate index.
curl ${ES_ENDPOINT}/license_plate/_search?pretty=true

# Perform a basic text search on the license_plate index.
curl ${ES_ENDPOINT}/license_plate/_search?pretty=true -H 'Content-Type: application/json' \
    -d @data/license_plate/term_query.json