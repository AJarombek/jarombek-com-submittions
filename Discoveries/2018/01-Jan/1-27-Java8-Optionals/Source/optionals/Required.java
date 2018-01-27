package optionals;

import java.util.Optional;

/**
 * Handle required fields in APIs
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
public class Required<T> {

    /**
     * Validate that a value exists otherwise throw an error
     * @param input - the value to check for existence
     * @return The input value if it exists
     */
    public T error(T input) {
        return Optional.ofNullable(input)
                .orElseThrow(() -> new Error("Unable to Set Required Field to null"));
    }

    /**
     * Validate that a value exists otherwise throw an error
     * @param input - the value to check for existence
     * @param name - the name of the value for error messaging purposes
     * @return The input value if it exists
     */
    public T error(T input, String name) {
        return Optional.ofNullable(input)
                .orElseThrow(() -> new Error("Unable to Set Required Field '" + name + "' to null"));
    }
}
