package optionals;

import java.util.Optional;

/**
 * Handle required fields in POJOs
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
public class Required<T> {

    public T error(T input) {
        return Optional.ofNullable(input)
                .orElseThrow(() -> new Error("Unable to Set Required Field to null"));
    }

    public T error(T input, String name) {
        return Optional.ofNullable(input)
                .orElseThrow(() -> new Error("Unable to Set Required Field '" + name + "' to null"));
    }
}
