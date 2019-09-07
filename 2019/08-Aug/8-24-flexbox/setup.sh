#!/usr/bin/env bash

# Setup to convert/compile Sass into CSS and start a web server hosting the HTML
# Author: Andrew Jarombek
# Since: 8/22/2019

# Start a compilation watcher to convert Sass to CSS
sass --version
sass --watch flex.scss:flex.css

# Start a web server locally to view the HTML/CSS from localhost
python -m http.server 81
# python3 -m http.server 81

# Navigate to - localhost:81/flex.html