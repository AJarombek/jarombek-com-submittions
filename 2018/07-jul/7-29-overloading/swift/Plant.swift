/*
 Struct representing a plant of a specific species
 - Author: Andrew Jarombek
 - Date: 7/28/2018
 */

import Foundation

struct Plant {
    
    let species: PlantSpecies
    let in_bloom: Bool?
    
    /**
     Constructor for a Plant of a specific species with a field determining whether or not
     the flower is in bloom.
     - parameters:
     - species: The specific species of the plant
     - in_bloom: Determing whether the plant is in bloom.  This argument can be ommited (nil)
     */
    init(species: PlantSpecies, in_bloom: Bool?) {
        self.species = species
        self.in_bloom = in_bloom
    }
    
    /**
     Constructor for a Plant of a specific species
     - parameters:
     - species: The specific species of the plant
     */
    init(species: PlantSpecies) {
        self.init(species: species, in_bloom: nil)
    }
}

extension Plant: Equatable {
    
    /**
     Extend the Plant class and implement the equality operator.  Checks if two plants can be
     determined as equal.  Plants are equal if their species and in_bloom fields are the same.
     - parameters:
     - lhs: the first plant to test for equality
     - rhs: the second plant to test for equality
     */
    static func ==(lhs: Plant, rhs: Plant) -> Bool {
        return lhs.in_bloom == rhs.in_bloom && lhs.species == rhs.species
    }
}

enum PlantSpecies {
    case Hosta, Daylily, Iris, Hibiscus, Peony
}
