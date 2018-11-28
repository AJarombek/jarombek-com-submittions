"""
Class representing a plant of a specific species
Author: Andrew Jarombek
Date: 7/28/2018
"""

from enum import Enum


class Plant(object):

    def __init__(self, species, in_bloom=False):
        """
        Constructor for a Plant of a specific species.  Optionally the plant has a flag
        for whether or not its flower is in bloom
        :param species: the specific species of the plant
        :param in_bloom: flag for whether or not the plant is in bloom.  This defaults to false
        """
        self.species = species
        self.in_bloom = in_bloom

    def __str__(self):
        """
        Create and return a human readable string that represents the Plant object
        :return: string
        """
        return '({}, {})'.format(self.species, self.in_bloom or None)

    def __repr__(self):
        """
        Create an internal representation of the Plant object.
        Simply delegate to the __str__() special method
        :return: string
        """
        return str(self)


class PlantSpecies(Enum):
    HOSTA = 1
    DAYLILY = 2
    IRIS = 3
    HIBISCUS = 4
    PEONY = 5
