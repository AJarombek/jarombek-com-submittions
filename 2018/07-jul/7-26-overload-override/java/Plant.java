import java.util.Optional;

/**
 * Class representing a plant of a specific species and an optional flag determining
 * if the flower is in bloom.  This class is immutable.
 * @author Andrew Jarombek
 * @since 7/19/2018
 */
public class Plant {

    enum Species {
        HOSTA, DAYLILY, IRIS, HIBISCUS, PEONY
    }

    private Plant.Species species;
    private Optional<Boolean> inBloom;

    private Plant(Plant.Species species, Boolean inBloom) {
        this.species = species;
        this.inBloom = Optional.ofNullable(inBloom);
    }

    public static Plant ofSpecies(Plant.Species species) {
        return new Plant(species, null);
    }

    public static Plant ofSpecies(Plant.Species species, boolean inBloom) {
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
