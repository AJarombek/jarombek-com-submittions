#!/usr/bin/env python

import pika
import sys

# Author: Andrew Jarombek
# Date: 1/18/2018
# Producer for a RabbitMQ topic exchange

connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
channel = connection.channel()

# Declare the topic_log exchange
channel.exchange_declare(exchange='topic_log', exchange_type='topic', passive=False,
                         durable=True, auto_delete=False)

routing_key = sys.argv[1] if len(sys.argv) > 2 else 'default'
message = ' '.join(sys.argv[2:]) or 'default'

# Set the content type for the RabbitMQ channel
# Also set the delivery mode to persistent - this allows the message to survive a crash
msg_props = pika.BasicProperties()
msg_props.content_type = 'text/plain'
msg_props.delivery_mode = 2

# Publish to the channel
# exchange -- the exchange to publish to
# routing_key -- the routing key to bind to
# properties -- basic properties of the message
# body -- the message contents
channel.basic_publish(exchange='topic_log', routing_key=routing_key,
                      properties=msg_props, body=message)
print(" [x] Sent %r:%r" % (routing_key, message))

connection.close()
