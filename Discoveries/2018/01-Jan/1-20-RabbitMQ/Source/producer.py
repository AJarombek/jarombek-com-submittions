#!/usr/bin/env python

import pika
import sys

# Author: Andrew Jarombek
# Date: 1/18/2018
# Producer for a RabbitMQ topic exchange

connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='topic_log', exchange_type='topic')

routing_key = sys.argv[1] if len(sys.argv) > 2 else 'default'
message = ' '.join(sys.argv[2:]) or 'default'

channel.basic_publish(exchange='topic_log', routing_key=routing_key, body=message)
print(" [x] Sent %r:%r" % (routing_key, message))

connection.close()