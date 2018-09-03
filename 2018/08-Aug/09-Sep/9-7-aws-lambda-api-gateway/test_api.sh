#!/usr/bin/env bash

# Test the API Gateway/AWS Lambda API that converts integers to roman numerals
# Author: Andrew Jarombek
# Date: 9/3/2018

# Convert 8 to VIII
curl https://xyhzuzr8z2.execute-api.us-east-1.amazonaws.com/dev/roman-numeral/8

# {"romanNumeral": "VIII"}

# Convert 28 to XXVIII
curl https://xyhzuzr8z2.execute-api.us-east-1.amazonaws.com/dev/roman-numeral/28

# {"romanNumeral": "XXVIII"}

# Convert 2013 to MMXIII
curl https://xyhzuzr8z2.execute-api.us-east-1.amazonaws.com/dev/roman-numeral/2013

# {"romanNumeral": "MMXIII"}