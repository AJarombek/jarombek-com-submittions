#!/usr/bin/env python

"""
Author: Andrew Jarombek
Date: 7/28/2018
"""

from Animal import Animal, AnimalSpecies
from Plant import Plant, PlantSpecies
from Garden import Garden

msGroundhog = Animal("Ms. Groundhog", AnimalSpecies.GROUNDHOG,
                     description="Enjoys lounging and eating grass all day")
mrGroundhog = Animal("Mr. Groundhog", AnimalSpecies.GROUNDHOG)

doe = Animal("doe", AnimalSpecies.DEER,
             description="from the tip of his wand burst the silver doe")
bunny = Animal("bunny", AnimalSpecies.RABBIT)

hosta = Plant(PlantSpecies.HOSTA)
lily = Plant(PlantSpecies.DAYLILY, in_bloom=True)
iris = Plant(PlantSpecies.IRIS, in_bloom=False)

garden = Garden(animals=[msGroundhog, mrGroundhog, doe, bunny],
                plants=[hosta, lily, iris])

assert garden.in_garden(doe)
assert garden.in_garden(lily)
assert not garden.in_garden(Animal("momma rabbit", AnimalSpecies.RABBIT))
