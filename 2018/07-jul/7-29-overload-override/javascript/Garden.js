/**
 * Representation of a Garden I do work in at Tod's Point
 * @author Andrew Jarombek
 * @since 7/27/2018
 */
class Garden {

    /**
     * Constructor for a Garden containing different animals, plants, and other objects.  All the
     * arguments for the constructor are optional and will be initialized as an empty array if
     * they are missing
     * @param animals - a list of animals in the garden (defaults to an empty array)
     * @param plants - a list of plants in the garden (defaults to an empty array)
     * @param randomObjects - a list of random objects in the garden (defaults to an empty array)
     */
    constructor(animals=[], plants=[], randomObjects=[]) {
        this.animals = animals;
        this.plants = plants;
        this.randomObjects = randomObjects;
    }

    /**
     * Check if an item exists in the garden.  If the argument passed in is non existent or not an
     * object, {false} is returned.  Through duck typing checks, if the object passed is an animal,
     * the internal {animals} array is checked.  If the object is a plant, the array {plants} is
     * checked.  Otherwise, the array {randomObjects} is checked.
     * @param item - an item that can be an animal, plant, or any random object
     * @return {boolean} whether or not the item exists in the garden
     */
    inGarden(item) {
        if (arguments.length === 0) {
            return false;
        }

        if (typeof item === "object") {

            // Check for the object type using duck typing
            if (item.hasOwnProperty("name") && item.hasOwnProperty("species")
                    && item.hasOwnProperty("description")) {
                return this.animals.includes(item);
            } else if (item.hasOwnProperty("species") && item.hasOwnProperty("inBloom")) {
                return this.plants.includes(item);
            } else {
                return this.randomObjects.includes(item);
            }

        } else {
            return false;
        }
    }
}

module.exports = Garden;