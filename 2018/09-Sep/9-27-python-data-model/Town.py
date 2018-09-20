"""
An object that represents a town
Author: Andrew Jarombek
Date: 9/19/2018
"""

from datetime import datetime


class Town:

    def __init__(self, name, founded, population=0, districts=list()):
        """
        Construct a town instance
        :param name: a string representing the name of the town
        :param founded: the date (integer) when the town was founded
        :param population: the current population (integer) of the town.  If no argument is
            supplied, the default population is zero.
        :param districts: all the separate governing bodies within the town.  Districts are
            represented as a list.  If no argument is supplied, an empty list is the default value.
        """
        self.name = name
        self.founded = founded
        self.population = population
        self.districts = districts

    def __repr__(self):
        """
        Special method used to get the string representation of an object.  It is invoked by
        both print() and str() among others.  This method is also a fallback from the __str__()
        special method
        :return: a String representation of the Town object
        """
        return 'Town of %s: Founded in %r with a Population of %r' % \
               (self.name, self.founded, self.population)

    def __len__(self):
        """
        Special method used to get the length of an object.  It is invoked by len()
        :return: the amount of time a town has been in existence.
        """
        return datetime.now().year - self.founded

    def __getitem__(self, item):
        """
        Special method to get a certain item at an index in the object.  It is invoked using
        the obj[index] syntax, and also enables iteration on the object.
        :param item: the index to search the object on
        :return: the district in the town at the index passed as an argument
        """
        return self.districts[item]

    def __eq__(self, other):
        """
        Special method to check for equality between objects.  It is invoked using the == syntax.
        __neq__() is implemented as the inverse of this method.
        :param other: another object to compare to this object
        :return: True if the objects are equal, False otherwise
        """
        return self.name == other.name \
            and self.founded == other.founded \
            and self.population == other.population \
            and self.districts == other.districts
