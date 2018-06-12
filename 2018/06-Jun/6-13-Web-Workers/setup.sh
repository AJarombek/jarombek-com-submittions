#!/usr/bin/env bash

yarn init

# Dependency which serves up a static web server.  This is needed to run web workers
yarn global add serve

# Spin up local web server
serve