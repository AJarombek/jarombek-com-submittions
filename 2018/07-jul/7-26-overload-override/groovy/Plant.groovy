package overloadride

/**
 * Class representing a plant of a specific species
 * @author Andrew Jarombek
 * @since 7/26/2018
 */
class Plant {

    enum Species {
        HOSTA, DAYLILY, IRIS, HIBISCUS, PEONY
    }

    Species species
    boolean inBloom
}
