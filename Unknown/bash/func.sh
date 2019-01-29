#!/usr/bin/env bash

# Create a function with Bash
# Author: Andrew Jarombek
# Date: 1/28/2019

mult() {
    local Str=""
    local Count=$2
    while [ ${Count} -gt 0 ]
    do
        Str="${Str}$1"

        # 'let' is used for arithmetic expressions
        let Count-=1
    done

    # Bash functions can't return values.  However, you can echo a local variable and capture it
    # at the location the function is invoked.
    echo ${Str}
}

# Must invoke a function after its declared
Cats=$(mult "cat" 3)
echo ${Cats}