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

# Same as previous command
echo $(ls)

# Arithmetic command
echo $((2 * 23))

# Get the scripts input parameters
Input1=$1
Input2=$2

# Validate input parameters using AND and OR commands
[ -z Input1 ] || (echo "First Argument NOT supplied." && exit 1)
[ -z Input2 ] || (echo "Second Argument NOT supplied." && exit 2)