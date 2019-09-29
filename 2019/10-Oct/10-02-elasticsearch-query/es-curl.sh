#!/usr/bin/env bash

# Commands for interacting with the ElasticSearch server
# Author: Andrew Jarombek
# Date: 9/28/2019

ES_ENDPOINT=https://search-sandbox-elasticsearch-demo-krzp4p5roknptbcoow2xlb5sby.us-east-1.es.amazonaws.com

# Delete, create, and retrieve an index for a race on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/race
curl -XPUT ${ES_ENDPOINT}/race -H 'Content-Type: application/json' -d @data/races/index.json
curl ${ES_ENDPOINT}/race?pretty=true

# Delete the documents in the race index
curl -XDELETE ${ES_ENDPOINT}/race/_doc/1
curl -XDELETE ${ES_ENDPOINT}/race/_doc/2
curl -XDELETE ${ES_ENDPOINT}/race/_doc/3
curl -XDELETE ${ES_ENDPOINT}/race/_doc/4
curl -XDELETE ${ES_ENDPOINT}/race/_doc/5
curl -XDELETE ${ES_ENDPOINT}/race/_doc/6

# Add documents to the race index
curl -XPUT ${ES_ENDPOINT}/race/_doc/1 -H 'Content-Type: application/json' \
    -d @data/races/westchester_tri.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/2 -H 'Content-Type: application/json' \
    -d @data/races/rail_trail.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/3 -H 'Content-Type: application/json' \
    -d @data/races/mohawk_mountain.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/4 -H 'Content-Type: application/json' \
    -d @data/races/hidden_valley.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/5 -H 'Content-Type: application/json' \
    -d @data/races/nyrr_cross_country.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/6 -H 'Content-Type: application/json' \
    -d @data/races/manchester_road_race.json

# Retrieve the documents in the race index
curl ${ES_ENDPOINT}/race/_doc/1?pretty=true
curl ${ES_ENDPOINT}/race/_doc/2?pretty=true
curl ${ES_ENDPOINT}/race/_doc/3?pretty=true
curl ${ES_ENDPOINT}/race/_doc/4?pretty=true
curl ${ES_ENDPOINT}/race/_doc/5?pretty=true
curl ${ES_ENDPOINT}/race/_doc/6?pretty=true

# Delete, create, and retrieve an index for testing character filters on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/test
curl -XPUT ${ES_ENDPOINT}/test -H 'Content-Type: application/json' -d @data/test/index.json
curl ${ES_ENDPOINT}/test?pretty=true