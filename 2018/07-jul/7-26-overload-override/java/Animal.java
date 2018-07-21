import java.util.Optional;

/**
 * Class representing an animal of a specific species along with a name.
 * This class is immutable.
 * @author Andrew Jarombek
 * @since 7/19/2018
 */
public class Animal {

    enum Species {
        DEER, GROUNDHOG, RABBIT, RACCOON, SQUIRREL, CHIPMUNK, CROW, CARDINAL
    }

    private String name;
    private Animal.Species species;
    private String description;

    private Animal(String name, Animal.Species species, String description) {
        this.name = name;
        this.species = species;
        this.description = description;
    }

    public static Animal ofName(String name, Animal.Species species) {
        return new Animal(name, species, null);
    }

    public static Animal ofNameAndDecscription(String name, String description, Animal.Species species) {
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
