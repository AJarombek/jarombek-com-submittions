/*
 Struct representing an animal of a specific species
 - Author: Andrew Jarombek
 - Date: 7/28/2018
 */

import Foundation

struct Animal {
    
    let name: String
    let species: AnimalSpecies
    let description: String?
    
    /**
     Constructor for an Animal of a specific species with a potentially empty description
     - parameters:
     - name: A string representing a name given to the animal
     - species: The specific species of the animal
     - description - A string describing the animal, which can be ommitted
     */
    init(name: String, species: AnimalSpecies, description: String?) {
        self.name = name
        self.species = species
        self.description = description
    }
    
    /**
     Constructor for an Animal of a specific species
     - parameters:
     - name: A string representing a name given to the animal
     - species: The specific species of the animal
     */
    init(name: String, species: AnimalSpecies) {
        self.init(name: name, species: species, description: nil)
    }
}

extension Animal: Equatable {
    
    /**
     Extend the Animal class and implement the equality operator.  Checks if two animals can be
     determined as equal.  Animals are equal if their name, species, and description fields are
     all the same
     - parameters:
     - lhs: the first animal to test for equality
     - rhs: the second animal to test for equality
     */
    static func ==(lhs: Animal, rhs: Animal) -> Bool {
        return lhs.name == rhs.name && lhs.species == rhs.species && lhs.description == rhs.description
    }
}

enum AnimalSpecies {
    case Deer, Groundhog, Rabbit, Raccoon, Squirrel, Chipmunk, Crow, Cardinal
}
