#!/usr/bin/env bash

# Author: Andrew Jarombek
# Date: 9/16/2018

npm install --save-dev @angular/cli@latest

ng new lifecycle-demo

ng set --global packageManager=yarn

cd lifecycle-demo/
ng serve

ng generate component home
ng generate component pictures