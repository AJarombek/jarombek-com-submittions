# Author: Andrew Jarombek
# Date: 11/14/2017
# Sort a list in Python with the sort() function and key function

people = [{"first":"Andrew", "last":"Jar."}, 
    {"first":"Thomas", "last":"Cau."},
    {"first":"Joe", "last":"Smi."},
    {"first":"Ben", "last":"Fis."}]

print(people)

# Function that accesses the value of the dictionary we are sorting on
def last(item):
    return item["last"]

# reverse optional argument can be removed
people.sort(key=last, reverse=False)

print(people)