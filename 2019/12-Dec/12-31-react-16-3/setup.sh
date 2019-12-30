#!/usr/bin/env bash

# CLI commands to setup the React 16.3 project.
# Author: Andrew Jarombek
# Date: 11/14/2019

npx --version
npx create-react-app react16-3

npm ls react-dom
npm ls react

cd react16-3
yarn

yarn start
