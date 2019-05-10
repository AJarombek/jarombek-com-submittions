#!/usr/bin/env bash

# Commands for interacting with the ElasticSearch server
# Author: Andrew Jarombek
# Date: 5/9/2019

# Add a document to the lp index and plate type
curl -XPUT domain/lp/plate/1 -d @ct_2010_passenger_lp.json