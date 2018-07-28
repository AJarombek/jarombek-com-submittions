/**
 * Class representing an animal of a specific species
 * @author Andrew Jarombek
 * @since 7/27/2018
 */
class Animal {

    /**
     * Constructor for an animal of a specific species with a name and optional description
     * of the animal
     * @param name - the name given to the animal
     * @param species - specific species of the animal
     * @param description - a description of the animal
     */
    constructor(name, species, description="") {
        this.name = name;
        this.species = species;
        this.description = description;
    }
}

Animal.Species = {
    DEER: 1,
    GROUNDHOG: 2,
    RABBIT: 3,
    RACCOON: 4,
    SQUIRREL: 5,
    CHIPMUNK: 6,
    CROW: 7,
    CARDINAL: 8
};

module.exports = Animal;