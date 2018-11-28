#!/usr/bin/env python

"""
Demonstrate the problems that arise from having a mutable default value for a constructor argument
Author: Andrew Jarombek
Date: 11/27/2018
"""

from Animal import Animal, AnimalSpecies
from Garden import Garden

# Create a bunny!!
bunny = Animal('bunny', AnimalSpecies.RABBIT)

# Add the bunny to a garden
garden1 = Garden([bunny])

# The string representation of the garden shows that it contains one animal (a bunny),
# no plants, and no random objects.
assert str(garden1) == '([(bunny, AnimalSpecies.RABBIT, None)], [], [])'

# Add a random object (myself) to the garden!
# Note that the internal random_object variable defaulted to an empty list
garden1.random_objects.append('Andy')

# Prove that 'Andy' was added to the garden
assert str(garden1) == '([(bunny, AnimalSpecies.RABBIT, None)], [], [\'Andy\'])'

# Create a new second garden
garden2 = Garden()

# OH NO! The new garden was supposed to be empty, but it contains 'Andy'.
# I accidentally mutated the default value for the random_objects argument!!
assert str(garden2) == '([], [], [\'Andy\'])'
