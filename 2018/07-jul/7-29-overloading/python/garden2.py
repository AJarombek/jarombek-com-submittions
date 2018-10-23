"""
Overload a function using the @singledispatch annotation.  Recreation of my previous discoveries
Python method overloading demo (https://bit.ly/2ypkf20)
Author: Andrew Jarombek
Date: 10/19/2018
"""

from functools import singledispatch
from Animal import Animal
from Plant import Plant

# Avoid shadowing by defining these variables as 'global' inside functions
_animals = []
_plants = []
_random_objects = []


def init_garden(animals=None, plants=None, random_objects=None):
    """
    Initialize a garden
    :param animals: animals that exist in the garden
    :param plants: plants that exist in the garden
    :param random_objects: random items that exist in the garden
    """

    # 'global' means the name exists at the module level.  'nonlocal' would mean that the variable
    # exists in the lexical scope of a function
    global _animals, _plants, _random_objects

    if plants is not None:
        _animals = list(animals)
    if plants is not None:
        _plants = list(plants)
    if random_objects is not None:
        _random_objects = list(random_objects)


@singledispatch
def in_garden(obj):
    """
    Check if a random item exists in the garden
    :param obj: a random item
    :return: True if the item exists, false otherwise
    """
    print("Searching the garden's random objects")
    return obj in _random_objects


@in_garden.register(Animal)
def _(animal):
    """
    Check if an animal exists in the garden
    :param animal: an animal
    :return: True if the animal exists, false otherwise
    """
    print("Searching the garden's animals")
    return animal in _animals


@in_garden.register(Plant)
def _(plant):
    """
    Check if a plant exists in the garden
    :param plant: a plant
    :return: True if the plant exists, false otherwise
    """
    print("Searching the garden's plants")
    return plant in _plants