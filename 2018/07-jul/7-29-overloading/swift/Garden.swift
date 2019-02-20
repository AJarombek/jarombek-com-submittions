/*
 Struct representing a garden that I do work in at Tod's Point.
 - Author: Andrew Jarombek
 - Date: 7/28/2018
 */

import Foundation

struct Garden {
    
    let animals: [Animal]
    let plants: [Plant]
    let random_objects: [Any]?
    
    /**
     Constructor for a garden with items in it.  These items include animals, plants, and
     optionally other random objects.
     - parameters:
     - withAnimals: An array of animals existing in the garden.
     - plants: An array of plants existing in the garden.
     - andObjects: An array of random objects existing in the garden.  This argument can be ommited (nil)
     */
    init(withAnimals animals: [Animal], plants: [Plant], andObjects objects: [Any]?) {
        self.animals = animals
        self.plants = plants
        random_objects = objects
    }
    
    /**
     Check if an animal exists in the garden.  Internally the animals array
     is checked for existance of the animal
     - parameters:
     - animal: an animal to look for in the garden
     */
    func inGarden(_ animal: Animal) -> Bool {
        return self.animals.contains { $0 == animal }
    }
    
    /**
     Check if a plant exists in the garden.  Internally the plants array
     is checked for existance of the plant
     - parameters:
     - plant: an plant to look for in the garden
     */
    func inGarden(_ plant: Plant) -> Bool {
        return self.plants.contains { $0 == plant }
    }
    
    /**
     Check if a random object exists in the garden.  Internally the random_objects array
     is checked for existance of the object
     - parameters:
     - object: a random object to look for in the garden
     */
    func inGarden(_ object: Any) -> Bool {
        let exists: Bool? = self.random_objects?.contains { $0 as? AnyObject === object as? AnyObject }
        return exists ?? false
    }
}
