import java.util.function.Consumer;

/**
 * Represent a user with different statuses.  These statuses are represented by a bit field
 * @author Andrew Jarombek
 * @since 6/22/2018
 */
public final class User {

    /* Flags for different statuses */
    static final int VALIDATED = 1;
    static final int SUBSCRIBED = 2;
    static final int ADMIN = 4;

    private int statusBitField;
    private String first;
    private String last;

    /**
     * Package-private constructor for the user.  The bit field is defaulted to a value of 0.
     * @param first - the users first name
     * @param last - the users last name
     */
    User(String first, String last) {
        statusBitField = 0;
        this.first = first;
        this.last = last;
    }

    /**
     * private constructor that takes in a bit field.
     * @param first - the users first name
     * @param last - the users last name
     * @param bitField - a bit field of the users statuses
     */
    private User(String first, String last, int bitField) {
        this.statusBitField = bitField;
        this.first = first;
        this.last = last;
    }

    /**
     * Add a status to the users bit field
     * @param user - an existing user object.  Users are treated as immutable - so a
     *             new user instance will be returned from this static method
     * @param status - a status to add to the bit field
     * @return a new user with an additional status compared to the one given as an argument
     */
    static User addStatus(User user, int status) {
        int updatedStatusBitField = user.statusBitField | status;

        return new User(user.first, user.last, updatedStatusBitField);
    }

    /**
     * Remove a status from the users bit field
     * @param user - an existing user object.  Users are treated as immutable - so a
     *             new user instance will be returned from this static method
     * @param status - a status to remove from the bit field
     * @return a new user with one less status compared to the one given as an argument
     */
    static User removeStatus(User user, int status) {
        int updatedStatusBitField = user.statusBitField & ~status;

        return new User(user.first, user.last, updatedStatusBitField);
    }

    /**
     * Perform an action (supplied by the second argument) when given a boolean
     * representing the existence of a status for the user.
     * @param mask - bit mask to flip the bits on the bit field
     * @param action - an action to perform with the boolean result of whether the status
     *               exists for the user or not.  This argument is a {@code Consumer} functional
     *               interface implementation, which is a lambda function that passes one argument
     *               and returns nothing.
     */
    void containsStatus(int mask, Consumer<Boolean> action) {
        action.accept(containsStatus(mask));
    }

    /**
     * Check for the existance of a status in a bit field
     * @param mask - bit mask to flip the bits on the bit field
     * @return {@code true} if the mask exists in the bit field, {@code false} otherwise
     */
    boolean containsStatus(int mask) {
        return (statusBitField & mask) > 0;
    }

    @Override
    public String toString() {
        return first + ", " + last + ", " + statusBitField;
    }
}
