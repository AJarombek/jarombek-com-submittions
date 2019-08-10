#!/usr/bin/env bash

# Demonstrate type equality in Bash.
# Author: Andrew Jarombek
# Date: 8/10/2019

# Print out a green successful message
# Source: [https://github.com/torokmark/assert.sh/blob/master/assert.sh#L22]
success() {
    local message=$1
    local echo_green=$(echo -en "\e[32m")
    local echo_white=$(echo -en "\e[00m")
    printf "${echo_green}✔ %s${echo_white}${message}\n"
}

# Print out a red failure message
failure() {
    local message=$1
    local echo_red=$(echo -en "\e[31m")
    local echo_white=$(echo -en "\e[00m")
    printf "${echo_red}✖ %s${echo_white}${message}\n"
}

# In the following examples, 'success' is always called.  'failure' is never called.

# In Bash everything is plain text except in certain contexts.  Both == and = test for text equality.
# = is defined in the POSIX spec while == is Bash specific.
if [ "andy" = "andy" ]
then
    success "[ 'andy' = 'andy' ]"
else
    failure "[ 'andy' = 'andy' ]"
fi

# = and == are synonyms
if [ "jarombek" == "jarombek" ]
then
    success "[ 'jarombek' == 'jarombek' ]"
else
    failure "[ 'jarombek' == 'jarombek' ]"
fi

# This would throw an exception - integer expression expected.
# [ "andy" -eq "andy" ]

# -eq tests for equality between numbers
if [ 2 -eq 2 ]
then
    success "[ 2 -eq 2 ]"
else
    failure "[ 2 -eq 2 ]"
fi

# Both == and = work for integers, but -eq doesn't work for strings
if [ 2 != 3 ]
then
    success "[ 2 != 3 ]"
else
    failure "[ 2 != 3 ]"
fi

# The double parenthesis construct $(( )) is used to perform arithmetic.
# Both of these comparisons throw errors.
# ((24 -eq 24))
# ((24 = 24))

if ((24 == 24))
then
    success "((24 == 24))"
else
    failure "((24 == 24))"
fi

# Using strings with double parenthesis compile but aren't evaluated properly.
if (("Greenwich,CT" != "greenwich,connecticut"))
then
    failure "((24 == 24))" # This is never called.  Don't use $(( )) to compare strings.
fi

# ... Unless if the strings are convertable to integers.
if (("10" == "10" && "5" != "6"))
then
    success "((\"10\" == \"10\" && \"5\" != \"6\"))"
else
    failure "((\"10\" == \"10\" && \"5\" != \"6\"))"
fi