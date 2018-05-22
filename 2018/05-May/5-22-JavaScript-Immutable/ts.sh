#!/usr/bin/env bash

# Globally install typescript
npm install -g typescript

# Compile .ts file to javascript
tsc immutableObjects.ts

# Run all files
node immutable.js
node immutableArray.js
node immutableObjects.js