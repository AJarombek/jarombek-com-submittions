/*
 Create a garden to demonstrate method overloading in Swift
 - Author: Andrew Jarombek
 - Date: 7/28/2018
 */

let msGroundhog = Animal(name: "Ms. Groundhog", species: AnimalSpecies.Groundhog,
                         description: "Enjoys lounging and eating grass all day")
let mrGroundhog = Animal(name: "Mr. Groundhog", species: AnimalSpecies.Groundhog)

let doe = Animal(name: "doe", species: AnimalSpecies.Deer,
                 description: "from the tip of his wand burst the silver doe")
let bunny = Animal(name: "bunny", species: AnimalSpecies.Rabbit)

let hosta = Plant(species: PlantSpecies.Hosta)
let lily = Plant(species: PlantSpecies.Daylily, in_bloom: true)
let iris = Plant(species: PlantSpecies.Iris, in_bloom: false)

let garden = Garden(withAnimals: [msGroundhog, mrGroundhog, doe, bunny], plants: [hosta, lily, iris], andObjects: nil)

assert(garden.inGarden(doe))

let otherDoe: Any = doe
assert(!garden.inGarden(otherDoe))

assert(garden.inGarden(lily))
assert(!garden.inGarden(Animal(name: "momma rabbit", species: AnimalSpecies.Rabbit)))
assert(!garden.inGarden("Squirrel"))
