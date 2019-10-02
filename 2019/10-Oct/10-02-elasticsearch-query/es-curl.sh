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

# Testing analyzers in Elasticsearch without an index
# Returns a single token with value "\nTitle\n"
curl -XPOST ${ES_ENDPOINT}/_analyze?pretty=true -H 'Content-Type: application/json' -d '{
  "tokenizer": "keyword",
  "char_filter": ["html_strip"],
  "text": "<h1>Title</h1>"
}'

# Returns a single token with value "Title"
curl -XPOST ${ES_ENDPOINT}/_analyze?pretty=true -H 'Content-Type: application/json' -d '{
  "tokenizer": "standard",
  "char_filter": ["html_strip"],
  "text": "<h1>Title</h1>"
}'

# Returns tokens: [Hello, my, name, is, Andy]
curl -XPOST ${ES_ENDPOINT}/_analyze?pretty=true -H 'Content-Type: application/json' -d '{
  "tokenizer": "standard",
  "text": "Hello my name is Andy."
}'

# Returns tokens: [Hello, my, name, is, Andy.]
curl -XPOST ${ES_ENDPOINT}/_analyze?pretty=true -H 'Content-Type: application/json' -d '{
  "tokenizer": "whitespace",
  "text": "Hello my name is Andy."
}'

# Returns tokens: [hello, my, name, is, andy]
curl -XPOST ${ES_ENDPOINT}/_analyze?pretty=true -H 'Content-Type: application/json' -d '{
  "tokenizer": "lowercase",
  "text": "Hello my name is Andy."
}'

# Delete, create, and retrieve an index for testing character filters on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/test
curl -XPUT ${ES_ENDPOINT}/test -H 'Content-Type: application/json' -d @data/test/index.json
curl ${ES_ENDPOINT}/test?pretty=true

# Run the test indexes analyzer on some strings
curl -XPOST ${ES_ENDPOINT}/test/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "emoji_analyzer",
  "text": "😀"
}'

curl -XPOST ${ES_ENDPOINT}/test/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "emoji_analyzer",
  "text": "🙂"
}'

curl -XPOST ${ES_ENDPOINT}/test/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "emoji_analyzer",
  "text": "🙁"
}'

curl -XPOST ${ES_ENDPOINT}/test/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "emoji_analyzer",
  "text": "😍"
}'

# Change the testing index to include multiple different custom tokenizers.
curl -XDELETE ${ES_ENDPOINT}/test
curl -XPUT ${ES_ENDPOINT}/test -H 'Content-Type: application/json' -d @data/test/indexV2.json

# Returns tokens: [andrew@jarombek.com, ajarombek95@gmail.com]
curl -XPOST ${ES_ENDPOINT}/test/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "email_analyzer",
  "text": "My emails are andrew@jarombek.com and ajarombek95@gmail.com."
}'

# Returns tokens: [Hi, my, is]
curl -XPOST ${ES_ENDPOINT}/test/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "short_words_analyzer",
  "text": "Hi my name is Andy Jarombek."
}'

# Returns tokens: [Dotty, good, horse]
curl -XPOST ${ES_ENDPOINT}/test/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "long_words_analyzer",
  "text": "Dotty is a good horse."
}'

# Delete, create, and retrieve an index with a custom analyzer for technologies on ElasticSearch.
curl -XDELETE ${ES_ENDPOINT}/tech
curl -XPUT ${ES_ENDPOINT}/tech -H 'Content-Type: application/json' -d @data/tech/index.json
curl ${ES_ENDPOINT}/tech?pretty=true

# Returns tokens: [Java]
curl -XPOST ${ES_ENDPOINT}/tech/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "tech_analyzer",
  "text": "Java"
}'

# Returns tokens: [Java, JavaS, avaS, ...]
curl -XPOST ${ES_ENDPOINT}/tech/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "tech_analyzer",
  "text": "JavaScript"
}'

# Returns tokens: [Node, Node., Node.j, Node.js, ode., ...]
curl -XPOST ${ES_ENDPOINT}/tech/_analyze -H 'Content-Type: application/json' -d '{
  "analyzer": "tech_analyzer",
  "text": "Node.js"
}'

# Delete all the documents in the 'tech' index
curl -XPOST ${ES_ENDPOINT}/tech/_delete_by_query -H 'Content-Type: application/json' -d '{
    "query": {
        "match_all": {}
    }
}'

# Create three documents in the 'tech' index
curl -XPOST ${ES_ENDPOINT}/tech/_doc -H 'Content-Type: application/json' -d \
    '{"name": "Elasticsearch"}'
curl -XPOST ${ES_ENDPOINT}/tech/_doc -H 'Content-Type: application/json' -d \
    '{"name": "Logstash"}'
curl -XPOST ${ES_ENDPOINT}/tech/_doc -H 'Content-Type: application/json' -d \
    '{"name": "Kibana"}'

# View all the documents in the 'tech' index
curl ${ES_ENDPOINT}/tech/_doc/_search?pretty=true -H 'Content-Type: application/json' -d '{
    "query": {
        "match_all": {}
    }
}'

# Match 'Elasticsearch'
curl ${ES_ENDPOINT}/tech/_doc/_search?pretty=true -H 'Content-Type: application/json' -d '{
    "query": {
        "match": {
            "name": "stic"
        }
    }
}'