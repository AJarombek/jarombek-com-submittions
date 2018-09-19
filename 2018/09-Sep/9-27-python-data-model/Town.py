"""
An object that represents a town
Author: Andrew Jarombek
Date: 9/19/2018
"""

from datetime import datetime


class Town:

    def __init__(self, name, founded, population=0, districts=list()):
        self.name = name
        self.founded = founded
        self.population = population
        self.districts = districts

    def __repr__(self):
        return 'Town of %s: Founded in %r with a Population of %r' % (self.name, self.founded, self.population)

    def __len__(self):
        return datetime.now().year - self.founded

    def __getitem__(self, item):
        return self.districts[item]
