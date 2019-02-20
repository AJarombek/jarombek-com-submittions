import java.util.Optional;

/**
 * Class representing a plant of a specific species and an optional flag determining
 * if the flower is in bloom.  This class is immutable.
 * @author Andrew Jarombek
 * @since 7/19/2018
 */
public class Plant {

    /**
     * Instead of having subclasses for each species, I made things easier by just using an enum
     * as a flag for the species.
     */
    enum Species {
        HOSTA, DAYLILY, IRIS, HIBISCUS, PEONY
    }

    private Plant.Species species;
    private Optional<Boolean> inBloom;

    /**
     * Private constructor for creating a plant.  Keeping the constructor private enforces
     * the use of static factory methods and helps ensure immutability by disabling subclassing
     * @param species the specific species for the plant
     * @param inBloom a flag that specifies if the plants flower is in bloom
     */
    private Plant(Plant.Species species, Boolean inBloom) {
        this.species = species;
        this.inBloom = Optional.ofNullable(inBloom);
    }

    /**
     * Static factory method for creating a new plant.  The plant instance created by this method
     * has no flag determining whether it is in bloom.
     * @param species the specific species for the plant
     * @return a new instance of {@code Plant}
     */
    static Plant ofSpecies(Plant.Species species) {
        return new Plant(species, null);
    }

    /**
     * Static factory method for creating a new plant with an in bloom flag.
     * @param species the specific species for the plant
     * @param inBloom a flag that specifies if the plants flower is in bloom
     * @return a new instance of {@code Plant}
     */
    static Plant ofSpecies(Plant.Species species, boolean inBloom) {
        return new Plant(species, inBloom);
    }

    /* Getters */

    public Optional<Boolean> getInBloom() {
        return inBloom;
    }

    public Species getSpecies() {
        return species;
    }
}
