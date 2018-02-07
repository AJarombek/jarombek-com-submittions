package optionals;

import java.util.Optional;

/**
 * @author Andrew Jarombek
 * @since 1/26/2018
 */
public class OptionalMethods {

    public static void main(String... args) {

        // Represent an Optional with an absent value
        Optional<String> empty = Optional.empty();
        System.out.println(empty); // Optional.empty

        // If the Optional value is present, perform the Consumer method reference
        Optional<String> present = Optional.of("I am not null!");
        present.ifPresent(System.out::println); // I am not null!
    }
}
