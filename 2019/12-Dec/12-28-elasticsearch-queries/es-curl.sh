#!/usr/bin/env bash

# Commands for making queries against the ElasticSearch server
# Author: Andrew Jarombek
# Date: 12/27/2019

ES_ENDPOINT=https://search-sandbox-elasticsearch-demo-krzp4p5roknptbcoow2xlb5sby.us-east-1.es.amazonaws.com

# Delete, create, and retrieve an index for a race on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/race
curl -XPUT ${ES_ENDPOINT}/race -H 'Content-Type: application/json' -d @data/race/index.json
curl ${ES_ENDPOINT}/race?pretty=true

# Delete the documents in the race index
curl -XDELETE ${ES_ENDPOINT}/race/_doc/1
curl -XDELETE ${ES_ENDPOINT}/race/_doc/2
curl -XDELETE ${ES_ENDPOINT}/race/_doc/3
curl -XDELETE ${ES_ENDPOINT}/race/_doc/4
curl -XDELETE ${ES_ENDPOINT}/race/_doc/5
curl -XDELETE ${ES_ENDPOINT}/race/_doc/6
curl -XDELETE ${ES_ENDPOINT}/race/_doc/7
curl -XDELETE ${ES_ENDPOINT}/race/_doc/8
curl -XDELETE ${ES_ENDPOINT}/race/_doc/9

# Add documents to the race index
curl -XPUT ${ES_ENDPOINT}/race/_doc/1 -H 'Content-Type: application/json' -d @data/race/race1.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/2 -H 'Content-Type: application/json' -d @data/race/race2.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/3 -H 'Content-Type: application/json' -d @data/race/race3.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/4 -H 'Content-Type: application/json' -d @data/race/race4.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/5 -H 'Content-Type: application/json' -d @data/race/race5.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/6 -H 'Content-Type: application/json' -d @data/race/race6.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/7 -H 'Content-Type: application/json' -d @data/race/race7.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/8 -H 'Content-Type: application/json' -d @data/race/race8.json
curl -XPUT ${ES_ENDPOINT}/race/_doc/9 -H 'Content-Type: application/json' -d @data/race/race9.json

# Retrieve the documents in the race index
curl ${ES_ENDPOINT}/race/_doc/1?pretty=true
curl ${ES_ENDPOINT}/race/_doc/2?pretty=true
curl ${ES_ENDPOINT}/race/_doc/3?pretty=true
curl ${ES_ENDPOINT}/race/_doc/4?pretty=true
curl ${ES_ENDPOINT}/race/_doc/5?pretty=true
curl ${ES_ENDPOINT}/race/_doc/6?pretty=true
curl ${ES_ENDPOINT}/race/_doc/7?pretty=true
curl ${ES_ENDPOINT}/race/_doc/8?pretty=true
curl ${ES_ENDPOINT}/race/_doc/9?pretty=true

# Query all the documents in the race index
curl ${ES_ENDPOINT}/race/_doc/_search?pretty=true -H 'Content-Type: application/json' -d @query/00-race-all.json

# Query for races that have a mile field with a value between 1 and 2, inclusive.
curl ${ES_ENDPOINT}/race/_doc/_search?pretty=true -H 'Content-Type: application/json' -d @query/01-race-short.json

# Query for short races (1-2 miles) in January.
curl ${ES_ENDPOINT}/race/_doc/_search?pretty=true -H 'Content-Type: application/json' -d @query/02-race-short-jan.json
curl ${ES_ENDPOINT}/race/_doc/_search?pretty=true -H 'Content-Type: application/json' -d '{}'