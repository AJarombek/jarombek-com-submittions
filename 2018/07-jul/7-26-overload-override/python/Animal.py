"""
Class representing an animal of a specific species
Author: Andrew Jarombek
Date: 7/28/2018
"""

from enum import Enum


class Animal(object):

    def __init__(self, name, species, description=""):
        """
        Construct an animal of a specific species with an optional description
        :param name: the name given to the animal
        :param species: specific species of the animal
        :param description: an optional description of the animal which defaults to
        an empty string
        """

        self.name = name
        self.species = species
        self.description = description


class AnimalSpecies(Enum):
    DEER = 1
    GROUNDHOG = 2
    RABBIT = 3
    RACCOON = 4
    SQUIRREL = 5
    CHIPMUNK = 6
    CROW = 7
    CARDINAL = 8
