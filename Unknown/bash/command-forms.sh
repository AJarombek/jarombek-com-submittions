#!/usr/bin/env bash

# Command forms in Bash
# Author: Andrew Jarombek
# Since: 1/29/2019

# Background commands '&'
ping -c 5 127.0.0.1 > /dev/null &

# Multiple commands on one line
cd .. ; ls

Shell="Access to Current Shell Variables"

# Execute commands in the current shell
{ echo ${Shell}; }

# Execute commands in a new shell.  Still has access to parent shell variables
( echo ${Shell}; )

Intro="My name is Andy."

# Pipes use output of one command to the input of another command.  In this example, the output of
# echo is passed to the input of grep.
echo ${Intro} | grep "Andy"

# Command substitution
echo `ls`