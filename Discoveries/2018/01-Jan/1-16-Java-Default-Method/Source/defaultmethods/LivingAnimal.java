package defaultmethods;

/**
 * @author Andrew Jarombek
 * @since 1/16/2018
 */
public interface LivingAnimal extends Animal {
    void run();
    void walk();
    void sleep();
    void eat();

    default String status() {
        return "I am a living animal!";
    }
}
