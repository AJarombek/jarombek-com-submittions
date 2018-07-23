import java.util.Optional;

/**
 * Class representing an animal of a specific species along with a name.
 * This class is immutable.
 * @author Andrew Jarombek
 * @since 7/19/2018
 */
public class Animal {

    /**
     * Instead of having subclasses for each species, I made things easier by just using an enum
     * as a flag for the species.
     */
    enum Species {
        DEER, GROUNDHOG, RABBIT, RACCOON, SQUIRREL, CHIPMUNK, CROW, CARDINAL
    }

    private String name;
    private Animal.Species species;
    private String description;

    /**
     * Private constructor for creating an animal.  Having the constructor private helps enforce
     * the objects immutability by disallowing subclasses
     * @param name a name given for the animal
     * @param species the specific species for the animal
     * @param description any additional information needed for the animal instance
     */
    private Animal(String name, Animal.Species species, String description) {
        this.name = name;
        this.species = species;
        this.description = description;
    }

    /**
     * Static factory method for creating a new animal.  The animal instance created by this method
     * has no description
     * @param name a name given for the animal
     * @param species the specific species for the animal
     * @return a new instance of {@code Animal}
     */
    static Animal ofName(String name, Animal.Species species) {
        return new Animal(name, species, null);
    }

    /**
     * Static factory method for creating a new animal with an additional description
     * @param name a name given for the animal
     * @param species the specific species for the animal
     * @param description any additional information needed for the animal instance
     * @return a new instance of {@code Animal}
     */
    static Animal ofName(String name, Animal.Species species, String description) {
        return new Animal(name, species, description);
    }

    /* Getters */

    public String getName() {
        return name;
    }

    public Species getSpecies() {
        return species;
    }

    public Optional<String> getDescription() {
        return Optional.ofNullable(description);
    }
}
