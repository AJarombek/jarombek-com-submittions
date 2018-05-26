#!/usr/bin/env bash

# Setup for the command line program
yarn init
npm install -g
yarn add commander

# Execute the command line program
random --help
random 0 1
random 1 10 -t int