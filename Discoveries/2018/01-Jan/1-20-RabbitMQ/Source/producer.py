#!/usr/bin/env python

import pika
from cred import Cred
from argparse import ArgumentParser
import json

# Author: Andrew Jarombek
# Date: 1/18/2018
# Producer for a RabbitMQ topic exchange

parser = ArgumentParser(description='Upload a New Log')

parser.add_argument("-WXC", action="append_const", dest="routing_keys", const="womensxc",
                    default=[], help="use the woman's cross country routing key")
parser.add_argument("-WTF", action="append_const", dest="routing_keys", const="womenstf",
                    default=[], help="use the woman's track & field routing key")
parser.add_argument("-MXC", action="append_const", dest="routing_keys", const="mensxc",
                    help="use the men's cross country routing key")
parser.add_argument("-MTF", action="append_const", dest="routing_keys", const="menstf",
                    help="use the men's track & field routing key")
parser.add_argument("-ALUM", action="append_const", dest="routing_keys", const="alumni",
                    help="use the alumni routing key")

parser.add_argument("-n", "--name", help="the name of the runner")
parser.add_argument("-d", "--distance", type=float, help="the distance run")
parser.add_argument("-m", "--metric", choices=["miles", "kilometers", "meters"], help="the distance run metric")
parser.add_argument("-t", "--time", help="the time taken on the run")
parser.add_argument("-dt", "--date", help="the date of the run 'yyyy-mm-dd'")
parser.add_argument("-l", "--location", help="the location of the run")
parser.add_argument("-des", "--description", help="a description of the run")

args = parser.parse_args()

json_object = dict()
json_object['name'] = args.name
json_object['distance'] = args.distance
json_object['metric'] = args.metric
json_object['time'] = args.time
json_object['date'] = args.date
json_object['location'] = args.location
json_object['description'] = args.description

json_data = json.dumps(json_object)


def build_routing_key(key_list):
    length = len(key_list) - 1
    str = ""

    for i in range(length):
        if i != 0:
            str += "."

        str += key_list[i]

    return str

routing_key = build_routing_key(args.routing_keys)
print(routing_key)

cred = Cred('producer')

# Set the connection parameters for the channel
credentials = pika.PlainCredentials(cred.USERNAME, cred.PASSWORD)
connection_params = pika.ConnectionParameters(cred.SERVER, virtual_host=cred.VHOST, credentials=credentials)

connection = pika.BlockingConnection(connection_params)
channel = connection.channel()

# Declare the running-log exchange
channel.exchange_declare(exchange=cred.EXCHANGE, exchange_type='topic', passive=False,
                         durable=True, auto_delete=False)

# Turn on confirm mode in the channel
channel.confirm_delivery()

# Set the content type for the RabbitMQ channel
# Also set the delivery mode to persistent - this allows the message to survive a crash
msg_props = pika.BasicProperties()
msg_props.content_type = 'text/plain'
msg_props.delivery_mode = 2

# Publish to the channel and check for publisher confirms
# exchange -- the exchange to publish to
# routing_key -- the routing key to bind to
# properties -- basic properties of the message
# body -- the message contents
if channel.basic_publish(exchange=cred.EXCHANGE, routing_key=routing_key, properties=msg_props,
                         body=json_data):
    print("Confirm Received!")
else:
    print("Message Received!")


print(" [x] Sending %r:%r" % (routing_key, json_data))

connection.close()
