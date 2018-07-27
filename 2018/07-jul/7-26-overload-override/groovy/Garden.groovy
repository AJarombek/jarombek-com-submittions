package overloadride

/**
 * Representation of a Garden I do work in at Tod's Point
 * @author Andrew Jarombek
 * @since 7/26/2018
 */
class Garden {

    List<Animal> animals
    List<Plant> plants
    List<Object> randomObjects

    /**
     * Check if the garden contains the provided animal
     * @param animal - an animal to search the classes animals property for
     * @return {@code true} if the garden contains the animal, {@code false} otherwise
     */
    def inGarden(Animal animal) {
        return animals.contains(animal)
    }

    /**
     * Check if the garden contains the provided plant
     * @param plant - a plant to search the classes plants property for
     * @return {@code true} if the garden contains the plant, {@code false} otherwise
     */
    def inGarden(Plant plant) {
        return plants.contains(plant)
    }

    /**
     * Check if the garden contains the provided random object
     * @param object - an object to search the classes random objects property for
     * @return {@code true} if the garden contains the random object, {@code false} otherwise
     */
    def inGarden(Object object) {
        return randomObjects.contains(object)
    }
}
