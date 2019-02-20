#!/usr/bin/env python

"""
Show the resolved mutable constructor default argument issue
Author: Andrew Jarombek
Date: 11/27/2018
"""

from Animal import Animal, AnimalSpecies
from GardenII import GardenII

# Create a deer!!
deer = Animal('sweetie', AnimalSpecies.DEER)

# Add the deer to a garden
garden1 = GardenII([deer])

# The string representation of the garden shows that it contains one animal (a deer),
# no plants, and no random objects.
assert str(garden1) == '([(sweetie, AnimalSpecies.DEER, None)], [], [])'

# Add a random object (myself) to the garden!
# Note that the internal random_object variable defaulted to None,
# and was assigned to an empty list in the constructor body
garden1.random_objects.append('Andy')

# Prove that 'Andy' was added to the garden
assert str(garden1) == '([(sweetie, AnimalSpecies.DEER, None)], [], [\'Andy\'])'

# Create a new second garden
garden2 = GardenII()

# The second garden is now empty as expected!
assert str(garden2) == '([], [], [])'
