/**
 * Class representing a plant of a specific species
 * @author Andrew Jarombek
 * @since 7/27/2018
 */
class Plant {

    /**
     * Constructor for a Plant of a specific species and a flag determining whether or not its
     * flower is in bloom
     * @param species - the specific species of the plant
     * @param inBloom - a flag for whether or not the flower is in bloom
     */
    constructor(species, inBloom) {
        this.species = species;
        this.inBloom = inBloom;
    }
}

Plant.Species = {
    HOSTA: 1,
    DAYLILY: 2,
    IRIS: 3,
    HIBISCUS: 4,
    PEONY: 5
};

module.exports = Plant;