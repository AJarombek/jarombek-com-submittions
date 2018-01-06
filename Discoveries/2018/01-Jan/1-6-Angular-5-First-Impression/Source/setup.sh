#!/usr/bin/env bash

npm install -g typescript

# Install the Angular CLI to make setup and configuration easy
npm install -g @angular/cli

ng version

# Create a new Angular project with name 'developer'
ng new developer

cd developer

# Start the Angular project on a server
ng serve

# ng serve throws an error in safari so perform this instead
# https://github.com/angular/angular-cli/issues/8400
ng serve --aot --no-sourcemap

ng generate directive valid-dev