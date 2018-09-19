#!/usr/bin/env bash

# Author: Andrew Jarombek
# Date: 9/16/2018

# Helpful command to remove node_modules from git!
git rm -r --cached 2018/09-Sep/9-26-react-angular-lifecycles/angular/lifecycle-demo/node_modules/

npm install --save-dev @angular/cli@latest

ng new lifecycle-demo

ng set --global packageManager=yarn

cd lifecycle-demo/
ng serve

ng generate component home
ng generate component pictures
ng generate component notification