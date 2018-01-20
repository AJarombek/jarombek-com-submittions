#!/usr/bin/env bash

# Author: Andrew Jarombek
# Date: 1/19/2017
# Get RabbitMQ set up for the project
# OS: MacOS Sierra

# Use homebrew on macOS to download erlang - the language rabbitMQ is written in
brew install erlang

# macOS equivalent of linux command 'wget'
#curl -O http://www.rabbitmq.com/releases/rabbitmq-server/v3.7.2/rabbitmq-server-generic-unix-3.7.2.tar.gz

# Or use homebrew
brew install rabbitmq

# Add the RabbitMQ server scripts to the PATH
PATH=$PATH:/usr/local/sbin

# Start up the rabbitmq server
rabbitmq-server

# Get the status of the rabbitmq node
rabbitmqctl status

python3 --version # 3.6.0

sudo pip3 install pika

# Add a new RabbitMQ user to consume messages
rabbitmqctl add_user running-log-consumer iconsume

# Add a new RabbitMQ user to produce messages
rabbitmqctl add_user running-log-producer iproduce

# List all the users on the RabbitMQ server
rabbitmqctl list_users

# Listing users ...
# running-log-producer	[]
# running-log-consumer	[]
# guest	[administrator]