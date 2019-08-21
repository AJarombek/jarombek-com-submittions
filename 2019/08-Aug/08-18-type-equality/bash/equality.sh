#!/usr/bin/env bash

# Demonstrate type equality in Bash.
# Sources: [https://stackoverflow.com/a/20449556, https://unix.stackexchange.com/a/168297,
#           https://unix.stackexchange.com/a/306115]
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

# The double parentheses construct $(( )) is used to perform arithmetic.
# Both of these comparisons throw errors.
# ((24 -eq 24))
# ((24 = 24))
# (("andy" -eq "andy"))
# (("andy" = "andy"))

if ((24 == 24))
then
    success "((24 == 24))"
else
    failure "((24 == 24))"
fi

# Using strings with double parentheses compile but aren't evaluated properly.
if (("Greenwich,CT" != "greenwich,connecticut"))
then
    failure "((24 == 24))" # This is never called.  Don't use $(( )) to compare strings.
fi

# ... Unless if the strings are convertible to integers.
if (("10" == "10" && "5" != "6"))
then
    success "((\"10\" == \"10\" && \"5\" != \"6\"))"
else
    failure "((\"10\" == \"10\" && \"5\" != \"6\"))"
fi

# Command substitution also throws an error.
# (24 == 24)
# ("andy" == "andy")

# Test equality of arrays
todays_workouts=("run" "walk" "kayak")
tomorrows_workouts=("run" "walk")
mondays_workouts=("run" "walk" "kayak")

# array[@] and array[*] both retrieve all the items in the array.  The difference between the two
# operators is noticeable when used in a for loop:
# https://linuxconfig.org/how-to-use-arrays-in-bash-script#h6-1-1-print-the-values-of-an-array
day1=${todays_workouts[@]}
day2=${tomorrows_workouts[@]}
day3=${mondays_workouts[*]}

# [[ conditions ]] has some extended features over [ conditions ], including the use of two
# conditions separated by an && operator!  Source: https://unix.stackexchange.com/a/32227
if [[ "${day1}" != "${day2}" && "${day1}" == "${day3}" ]]
then
    success "[[ Arrays Equality ]]"
else
    failure "[[ Arrays Equality ]]"
fi