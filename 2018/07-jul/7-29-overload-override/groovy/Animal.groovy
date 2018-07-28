package overloadride

/**
 * Class representing an animal of a specific species
 * @author Andrew Jarombek
 * @since 7/26/2018
 */
class Animal {

    enum Species {
        DEER, GROUNDHOG, RABBIT, RACCOON, SQUIRREL, CHIPMUNK, CROW, CARDINAL
    }

    String name
    Species species
    def description

    /**
     * Constructor for the animal with an additional description.  Explicitly declaring a constructor
     * allows for object instances to be constructed using the Groovy list syntax
     * @param name - name to be given to the animal
     * @param species - specific species of the animal
     * @param description - additional description of the animal
     */
    Animal(name, species, description) {
        this.name = name
        this.species = species
        this.description = description
    }

    /**
     * Constructor for the animal without an additional description
     * @param name - name to be given to the animal
     * @param species - specific species of the animal
     */
    Animal(name, species) {
        this(name, species, null)
    }
}
