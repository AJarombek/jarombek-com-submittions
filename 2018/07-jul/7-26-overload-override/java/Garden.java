import java.util.ArrayList;
import java.util.List;

/**
 * Class representing a garden that I do work in at Tod's Point.  The class is used to demonstrate the
 * differences between overloading and overriding.  It is also immutable.
 * @author Andrew Jarombek
 * @since 7/21/2018
 */
public final class Garden {

    private List<Animal> animals;
    private List<Plant> plants;
    private List<Object> randomObjects;

    /**
     * Private constructor to disallow subclassing and enforce the use of static factory methods
     * @param animals a group of animals, the class must implement {@code Iterable}
     * @param plants a group of plants, the class must implement {@code Iterable}
     * @param randomObjects a group of random objects, the class must implement {@code Iterable}
     */
    private Garden(Iterable<Animal> animals, Iterable<Plant> plants, Iterable<?> randomObjects) {
        this.animals = new ArrayList<>();
        this.plants = new ArrayList<>();
        this.randomObjects = new ArrayList<>();

        animals.forEach(this.animals::add);
        plants.forEach(this.plants::add);
        randomObjects.forEach(this.randomObjects::add);
    }

    /**
     * Static factory method to create a Garden.  No random objects will exist in the garden.
     * @param animals a group of animals, the class must implement {@code Iterable}
     * @param plants a group of plants, the class must implement {@code Iterable}
     * @return a new Garden instance
     */
    static Garden of(Iterable<Animal> animals, Iterable<Plant> plants) {
        return new Garden(animals, plants, List.of());
    }

    /**
     * Static factory method to create a Garden.  No random objects will exist in the garden.
     * @param animals a group of animals, the class must implement {@code Iterable}
     * @param plants a group of plants, the class must implement {@code Iterable}
     * @param randomObjects a group of random objects, the class must implement {@code Iterable}
     * @return a new Garden instance
     */
    static Garden of(Iterable<Animal> animals, Iterable<Plant> plants, Iterable<?> randomObjects) {
        return new Garden(animals, plants, randomObjects);
    }

    /**
     * Check if the garden instance contains the provided animal
     * @param animal an animal to search the internal list structure for
     * @return {@code true} if the garden contains the animal, {@code false} otherwise
     */
    boolean inGarden(Animal animal) {
        System.out.println("Checking if Animal in Garden");
        return animals.contains(animal);
    }

    /**
     * Check if the garden instance contains the provided plant
     * @param plant a plant to search the internal list structure for
     * @return {@code true} if the garden contains the plant, {@code false} otherwise
     */
    boolean inGarden(Plant plant) {
        System.out.println("Checking if Plant in Garden");
        return plants.contains(plant);
    }

    /**
     * Check if the garden instance contains the provided object
     * @param object a random object to search the internal list structure for
     * @return {@code true} if the garden contains the object, {@code false} otherwise
     */
    boolean inGarden(Object object) {
        System.out.println("Checking if Random Object in Garden");
        return randomObjects.contains(object);
    }
}
