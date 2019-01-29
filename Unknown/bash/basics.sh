#!/usr/bin/env bash

# Bash script basics, mirrors the Batch script basics (https://bit.ly/2DE8SGF)
# Usage: bash basics.sh <argument>
# Author: Andrew Jarombek
# Date: 1/27/2019

bash --version

# The first argument passed into the script
echo "The first argument passed: $1"

Name="Andrew Jarombek"
echo ${Name}

# Extract the characters after index 7
LastName=${Name:7}
echo ${LastName}

# Extract the characters from index 0 to index 6
FirstName=${Name:0:6}
echo ${FirstName}

# Replace Jarombek with Jarbek
AltName=${Name/Jarombek/Jarbek}
echo ${AltName}

# Addition is simpler in Bash compared to Batch
# $((...)) syntax creates an arithmetic substitution
Age=23
AgeInTenYears=$(($Age + 10))
echo ${AgeInTenYears}

# In Bash, variables defined in functions are default global
func() {
    Age=24
    echo ${Age}
}

# Prints: 24
func

# func affects the global scoped Age variable.  Prints: 24
echo ${Age}

# Define local and global variables
func2() {
    Global="Global Variable"
    local Local="Local Variable"

    # Both global and local print
    echo "${Global}, ${Local}"
}

func2

# Only global prints
echo "${Global}, ${Local}"

# System variable
echo ${PATH}

# Create an array and assign values to indices
Towns=()
Towns[0]=Greenwich
Towns[1]="New Canaan"
Towns[2]=Darien
Towns[3]=Wilton
Towns[4]=Ridgefield

echo ${Towns[0]}

# Bash also has associative arrays (These only work in Bash Version 4)
declare -A SkiLocations=([sat]="Catamount" [sun]="Jiminy Peak")
echo ${SkiLocations[sat]}

# Create an array of integers
NumberList=(1 2 3 4 5)

# Get all the items from NumberList
echo ${NumberList[*]}

# Accumulate all the strings in the town list
for i in ${NumberList[*]}
do
    AllTowns="${AllTowns} ${Towns[$((i - 1))]}"
done

echo ${AllTowns}

# Accumulate some of the strings in the town list
for i in 2 4
do
    SomeTowns="${SomeTowns} ${Towns[$((i - 1))]}"
done

echo ${SomeTowns}

# Arrays in Bash are one dimensional, unlike Batch where array elements can be complex structures

# Get the current date
echo date

# Create a unix timestamp of a date on MacOS
DateWritten=$(date -j -f "%F" 2019-01-28 +"%s")
MonthsEnd=$(date -j -f "%F" 2019-01-31 +"%s")

# Equivalent on Linux
# DateWritten=$(date -d 2019-01-28)

if [ ${DateWritten} -lt ${MonthsEnd} ]
then
    echo "Date is before Jan. 31st, 2019"
else
    echo "Date is equal to or after Jan. 31st, 2019"
fi