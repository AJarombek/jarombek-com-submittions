#!/usr/bin/env python

import pika
import sys

# Author: Andrew Jarombek
# Date: 1/18/2018
# Consumer for a RabbitMQ topic exchange

connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
channel = connection.channel()

# Create an exchange if it does not already exist, otherwise make sure it has expected attributes
# exchange -- the name of the exchange
# exchange_type -- the type of exchange, in this case a topic exchange
# passive -- perform an exchange declare (False) or just check if it exists (True)
# durable -- If the exchange will survive a RabbitMQ reboot
# auto_delete -- If the exchange will be removed when no queues are bound to it
channel.exchange_declare(exchange='topic_log', exchange_type='topic', passive=False,
                         durable=True, auto_delete=False)

# Declare a queue and create it if needed
# queue --  the name of the queue - if you dont specify a name RabbitMQ will auto create one
# exclusive -- only allow access by the current connection
# durable -- Survives a reboot
result = channel.queue_declare(queue='log', exclusive=True, durable=True)
queue_name = result.method.queue

# Use command line arguments to get the binding keys
binding_keys = sys.argv[1:]
if not binding_keys:
    sys.stderr.write("Usage: %s [binding_key]...\n" % sys.argv[0])
    sys.exit(1)

# Bind this channel to potentially many queues
for binding_key in binding_keys:

    # Bind a queue to a specific exchange
    # exchange -- the exchange to bind to
    # queue -- the queue to bind to the exchange
    # routing_key -- the routing key to bind to - this is for use in a topic exchange
    channel.queue_bind(exchange='topic_log', queue=queue_name, routing_key=binding_key)

print(" [*] Waiting for logs.  To exit press CTRL+C")


def consumer(ch, method, properties, body):
    """ Consumer callback is called when a message is consumed """
    if body == 'quit':

        # Cancel a consumer
        # consumer_tag -- id for the consumer
        ch.basic_cancel(consumer_tag='consumer')
        ch.stop_consuming()
    else:
        print(" [x] %r:%r" % (method.routing_key, body))
    return

# Start a queue consumer and bind messages to the callback function
# queue -- the name of the queue to consume from
# consumer_tag -- the id of the consumer
# no_ack -- tell the broker not to expect a response back
channel.basic_consume(consumer, queue=queue_name, consumer_tag='consumer', no_ack=True)
channel.start_consuming()
