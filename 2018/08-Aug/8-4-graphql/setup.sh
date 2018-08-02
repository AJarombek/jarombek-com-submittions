#!/usr/bin/env bash

# Author: Andrew Jarombek
# Date: 8/1/2018
# Setup commands for creating a GraphQL and Node.js project

npm init
yarn add graphql

# Run the basic 'Hello World' sample code for GraphQL
node index.js

# Set up a web server for GraphQL
yarn add express express-graphql