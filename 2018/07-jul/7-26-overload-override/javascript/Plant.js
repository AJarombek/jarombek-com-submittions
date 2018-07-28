class Plant {

    constructor(species, inBloom=undefined) {
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