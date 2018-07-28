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


class PlantSpecies(Enum):
    HOSTA = 1
    DAYLILY = 2
    IRIS = 3
    HIBISCUS = 4
    PEONY = 5
