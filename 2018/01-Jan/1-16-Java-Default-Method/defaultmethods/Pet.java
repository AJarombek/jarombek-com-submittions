package defaultmethods;

/**
 * @author Andrew Jarombek
 * @since 1/16/2018
 */
public interface Pet extends Animal {
    String owner();

    default String status() {
        return "I am a pet!";
    }
}
