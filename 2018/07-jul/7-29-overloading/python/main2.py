"""
Main module to test out the Garden API
Author: Andrew Jarombek
Date: 10/22/2018
"""

from Plant import Plant, PlantSpecies
from Animal import Animal, AnimalSpecies
from garden import init_garden, in_garden

msGroundhog = Animal("Ms. Groundhog", AnimalSpecies.GROUNDHOG)
mrGroundhog = Animal("Mr. Groundhog", AnimalSpecies.GROUNDHOG)

doe = Animal("doe", AnimalSpecies.DEER, description="from the tip of his wand burst the silver doe")
bunny = Animal("bunny", AnimalSpecies.RABBIT)

hosta = Plant(PlantSpecies.HOSTA)
lily = Plant(PlantSpecies.DAYLILY, in_bloom=True)
iris = Plant(PlantSpecies.IRIS, in_bloom=False)

garden = init_garden(animals=[msGroundhog, mrGroundhog, doe, bunny], plants=[hosta, lily, iris])

assert in_garden(doe)   # Searching the garden's animals
assert in_garden(lily)  # Searching the garden's plants
assert not in_garden(Animal("momma rabbit", AnimalSpecies.RABBIT))  # Searching the garden's animals
assert not in_garden(True)  # Searching the garden's random objects