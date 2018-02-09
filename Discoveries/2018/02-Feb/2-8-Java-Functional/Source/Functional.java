import java.util.Optional;

/**
 * Show how to use an Optional instead of throwing an Error
 * @author Andrew Jarombek
 * @since 2/8/2018
 */
public class Functional {

    public static Optional<Double> divide(Double numerator, Double denominator) {

        // Wrap arguments into optionals
        Optional<Double> num = Optional.ofNullable(numerator);
        Optional<Double> den = Optional.ofNullable(denominator);

        // Return division result if arguments exist and the denominator isn't zero, otherwise return empty optional
        return (!num.isPresent() || !den.isPresent()) ?
                Optional.empty() : (den.get() == 0) ?
                Optional.empty() : Optional.of(num.get() / den.get());
    }

    public static void main(String... args) {
        Optional<Double> success = divide(5.0, 2.0);
        Optional<Double> fail = divide(5.0, 0.0);
        Optional<Double> failBecauseNull = divide(null, 6.5);

        success.ifPresent(System.out::println);

        // Both of these Optionals will be empty
        if (!fail.isPresent()) {
            System.out.println("Can't Divide By Zero");
        }

        if (!failBecauseNull.isPresent()) {
            System.out.println("Can't Pass an Empty Value");
        }
    }
}
