"""
Representation of a Garden I do work in at Tod's Point
Author: Andrew Jarombek
Date: 7/28/2018
"""

from Animal import Animal
from Plant import Plant


class Garden(object):

    def __init__(self, animals=list(), plants=list(), random_objects=list()):
        """
        Constructor for a garden which contains animals, plants, and some other random objects
        :param animals: a list of animals in the garden (defaults to an empty list)
        :param plants: a list of plants in the garden (defaults to an empty list)
        :param random_objects: a list of random objects in the garden (defaults to an empty list)
        """
        self.animals = animals
        self.plants = plants
        self.random_objects = random_objects

    def in_garden(self, item):
        """
        Check whether an item exists in the garden.  The item can be a plant, animal, or any
        random object.  Depending on the type of the object, a different internal list will be
        checked for the existence of the object.
        :param item: the item to check for existence in the garden
        :return: False if the item is None or doesnt exist in the garden, True otherwise
        """

        if item is not None:
            if type(item) is Animal:
                return item in self.animals
            if type(item) is Plant:
                return item in self.plants
            else:
                return item in self.random_objects
        else:
            return False

    def __str__(self):
        """
        Create and return a human readable string that represents the Garden object
        :return: string
        """
        return '({}, {}, {})'.format(self.animals, self.plants, self.random_objects)

    def __repr__(self):
        """
        Create an internal representation of the Garden object.
        Simply delegate to the __str__() special method
        :return: string
        """
        return str(self)
