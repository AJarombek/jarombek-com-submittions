#!/usr/bin/env groovy

package overloadride

/**
 * Create a garden to demonstrate method overloading in Groovy
 * @author Andrew Jarombek
 * @since 7/26/2018
 */

def groundhog = Animal.Species.GROUNDHOG
def deer = Animal.Species.DEER
def rabbit = Animal.Species.RABBIT

def msGroundhog = new Animal("Ms. Groundhog", groundhog, "Enjoys lounging and eating grass all day")
def mrGroundhog = new Animal("Mr. Groundhog", groundhog)

def doe = ["doe", deer, "from the tip of his wand burst the silver doe"] as Animal
def bunny = ["bunny", rabbit] as Animal

def hosta = new Plant(species: Plant.Species.HOSTA)
def lily = new Plant(species: Plant.Species.DAYLILY, inBloom: true)
def iris = new Plant(species: Plant.Species.IRIS, inBloom: false)

def garden = new Garden(animals: [msGroundhog, mrGroundhog, doe, bunny], plants: [hosta, lily, iris])

assert garden.inGarden(doe)

// Groovy has multi methods and runtime method dispatching - so this method will call the
// overloaded method with the appropriate runtime type - which is Animal NOT Object
Object objectDoe = doe
assert garden.inGarden(objectDoe)