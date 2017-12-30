!/usr/bin/env bash

gem install sass

# Watch the sass file for changes and convert it to css
sass --watch Sass/blog-discovery.scss:blog-discover.css