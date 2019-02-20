#!/usr/bin/env bash

# Author: Andrew Jarombek
# Date: 6/11/2018
# Spin up a local server to run the web workers

yarn init

# Dependency which serves up a static web server.  This is needed to run web workers
yarn global add serve

# Spin up local web server
serve