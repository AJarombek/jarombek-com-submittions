"""
Experimenting with Different Python Data Structures
Author: Andrew Jarombek
Date: 9/27/2018
"""

from datetime import date
from collections import namedtuple
from bisect import insort

"""
Lists
"""

# Convert kilometers to miles - performs a map operations with a listcomp
miles = [str('%1.2f' % (km * 0.621371)) for km in [5, 10, 15]]

print(miles)
assert miles == ['3.11', '6.21', '9.32']

# Perform a filter operation with a listcomp
runs = [5, 3.4, 4.95, 11, 6, 14.5]
long_runs = [r for r in runs if r >= 10]

print(long_runs)
assert long_runs == [11, 14.5]

twentyK = 0

# Use a generator expression instead of a listcomp so that the kilometers are never built to memory
# Generator syntax use () instead of a listcomps []
for run in (str('%1.2f' % (km * 0.621371)) for km in [5, 10, 15]):
    twentyK += float(run)

print(twentyK)
assert twentyK == 18.64

sorted_runs = sorted(runs)
assert sorted_runs == [3.4, 4.95, 5, 6, 11, 14.5]

# Use insort to insert into a sorted list, maintaining the sorted order
insort(sorted_runs, 8.5)
assert sorted_runs == [3.4, 4.95, 5, 6, 8.5, 11, 14.5]

"""
Tuples
"""

# Use tuple unpacking to pass each tuple record as an argument to a function
# Tuple unpacking uses the * syntax
items = ("My", "name", "is", "Andy")

print(items)   # ('My', 'name', 'is', 'Andy')
print(*items)  # My name is Andy

_my, _name, _is, _andy = items
assert _my == "My" and _name == "name" and _is == "is" and _andy == "Andy"

run_info = (4.32, 31, 25, 'Riverside, CT', date(2018, 9, 27))

# Tuple unpacking can also be used to grab excess items.
# Records can be grabbed from the end of a tuple...
miles, minutes, seconds, *rest = run_info

assert miles == 4.32 and minutes == 31 and seconds == 25
assert rest == ['Riverside, CT', date(2018, 9, 27)]

# ...or the start of a tuple...
*stats, location, run_date = run_info

assert stats == [4.32, 31, 25]
assert location == 'Riverside, CT' and run_date == date(2018, 9, 27)

# ...or the middle of a tuple
miles, *others, run_date = run_info

assert others == [31, 25, 'Riverside, CT']

"""
Named Tuple
"""

Run = namedtuple('Run', 'miles minutes seconds location run_date')

yesterdays_run = Run(4.95, 34, 12, 'Riverside, CT', date(2018, 9, 26))

run_desc = '{0} miles in {1}:{2}'.format(yesterdays_run.miles,
                                         yesterdays_run.minutes,
                                         yesterdays_run.seconds)
print(run_desc)
assert run_desc == '4.95 miles in 34:12'

"""
Memory View
"""

# A byte array is a mutable binary sequence.  The built-in 'byte' type is immutable
name_byte_array = bytearray('Andy', 'utf-8')

# Memory views take in an object that exposes the buffer protocol
memv = memoryview(name_byte_array)

# Analyze each byte in the memory view
print(memv)
print(memv[0], memv[1], memv[2], memv[3])
print(chr(memv[0]), chr(memv[1]), chr(memv[2]), chr(memv[3]))

assert memv[0] == 65 and memv[1] == 110 and memv[2] == 100 and memv[3] == 121
assert chr(memv[0]) == 'A' and chr(memv[1]) == 'n' and chr(memv[2]) == 'd' and chr(memv[3]) == 'y'

assert name_byte_array == bytearray(b'Andy')

# Convert the first byte in the memory view from capital 'A' to lowercase 'a'
memv[0] = 97

# Confirm the change to the memory view impacts the original array
assert name_byte_array == bytearray(b'andy')

# Split the memory view, creating a new memory view that holds only the last two bytes
last_two_letters_memv = memv[2:]

assert bytes(last_two_letters_memv) == b'dy'

# From the split memory view, change the last letter to 'i'
last_two_letters_memv[1] = ord('i')

# Since the split memory view didn't copy any bytes, the original byte array is still impacted
assert name_byte_array == bytearray(b'andi')

"""
Sets
"""

s1 = {1, 2, 3}
s2 = {3, 4, 5}

# Bitwise and is used to get the Set intersection
assert s1 & s2 == {3}

# Bitwise or is used to get the Set union
assert s1 | s2 == {1, 2, 3, 4, 5}

# Subtraction sign is used to get the Set difference
assert s1 - s2 == {1, 2}
assert s2 - s1 == {4, 5}

# Bitwise not is used to get the complement of the Set intersection
assert s1 ^ s2 == {1, 2, 4, 5}

s3 = {22, 44, 66}
s4 = {22}

# Determine whether a set is a subset or superset of another set with the comparison operators

assert not s3 <= s4  # s3 is not a subset of s4
assert not s3 < s4   # s3 is not a proper subset (subset but not matching set) of s4
assert s3 >= s4      # s3 is a superset of s4
assert s3 > s4       # s3 is a proper superset (superset but not matching set) of s4
