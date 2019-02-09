#!/usr/bin/env bash

# Experimenting with commonly used bash commands
# Author: Andrew Jarombek
# Date: 2/7/2019

# grep (globally search a regular expression and print)

echo "** Find all the lines containing a dollar sign in 'func.sh'"
cat func.sh | grep '\$'

echo "** Same as above"
grep '\$' func.sh

echo "** Find all the lines that end with ']'.  '$' Denotes the end of a line"
grep ']$' func.sh

echo "** Find all the lines the access elements in the Towns array"
grep 'Towns\[[0-9+]\]' basics.sh

echo "** Count the occurrences of '$' in 'basics.sh'"
grep -c '\$' basics.sh

# cat (concatenate)

echo "** Redirect the output of the 'cat' command to the null device"
cat func.sh > /dev/null

echo "** Write the 'func.sh' file to stdout with line numbers"
cat -n func.sh; echo

# head (head end of a file)

echo "** Display the first 10 lines of 'basics.sh'"
head basics.sh

echo "** Just display the shebang line"
head -n 1 basics.sh

echo "** Just display the shebang"
head -c 2 basics.sh; echo

echo "** The two most recently edited files"
ls -t | head -n 2

# tail (tail end of a file)

echo "** Display the last 10 lines of 'basics.sh'"
tail basics.sh; echo

echo "** Display the last line of 'basics.sh'"
tail -n 1 basics.sh; echo

# echo "** Continue the stream until the user breaks it"
# tail -f basics.sh