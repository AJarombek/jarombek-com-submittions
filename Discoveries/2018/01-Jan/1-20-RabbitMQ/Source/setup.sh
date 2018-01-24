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

# Set users permissions to vhost "/": Read, Write, Configure
# "" means no permission, ".*" means permissions granted to all queues and exchanges
rabbitmqctl set_permissions running-log-consumer ".*" ".*" ".*"
rabbitmqctl set_permissions running-log-producer ".*" ".*" ".*"

# List all of the users and their permissions
rabbitmqctl list_permissions

# Look at all queues
rabbitmqctl list_queues

# Set up RabbitMQ consumers - the argument is the routing key to bind the consumer to
python3 ./consumer.py womensxc.*
python3 ./consumer.py womensxc.womenstf
python3 ./consumer.py #

# Set up a RabbitMQ producer

# Ask for help with descriptions of the command line arguments
python3 ./producer.py --help

# An example producer
python3 ./producer.py -MXC -MTF -ALUM --name Andy --distance 3.05 --metric miles \
            --date 2018-01-24 --location "Riverside CT"

# [x] Sending 'mensxc.menstf.alumni':'{"name": "Andy", "distance": 3.05, "metric": "miles", "time": null,
#       "date": "2018-01-24", "location": "Riverside CT", "description": null}'