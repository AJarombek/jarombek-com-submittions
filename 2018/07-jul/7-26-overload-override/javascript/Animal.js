class Animal {

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