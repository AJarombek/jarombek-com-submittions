package defaultmethods;

/**
 * @author Andrew Jarombek
 * @since 1/16/2018
 */
public interface Animal {

    String aboutMe();

    /**
     * Default method which will be called if this method doesn't exist in the implemented class
     * @return the age of the animal
     */
    default int age() {
        return 0;
    }

    /**
     * In Java 8 interfaces can have static methods
     * @return a description of this animal
     */
    static String info() {
        return "I am an animal";
    }
}
