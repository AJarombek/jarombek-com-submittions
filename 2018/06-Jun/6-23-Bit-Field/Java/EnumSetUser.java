import java.util.Collection;
import java.util.EnumSet;
import java.util.function.Consumer;

/**
 * Represent a user with different statuses.  These statuses are represented by an enum set.  In Java,
 * the EnumSet interface has the same efficiency as using bit fields while also being type safe.  It is
 * the recommended approach
 * @author Andrew Jarombek
 * @since 6/22/2018
 *
 * @see EnumSet
 */
public final class EnumSetUser {

    /* Flags for different statuses */
    enum Status {
        VALIDATED, SUBSCRIBED, ADMIN
    }

    private EnumSet<Status> statusEnumSet;
    private String first;
    private String last;

    /**
     * Package-private constructor for the user.  The {@code EnumSet} is empty by default.
     * @param first - the users first name
     * @param last - the users last name
     */
    EnumSetUser(String first, String last) {
        this.statusEnumSet = EnumSet.noneOf(Status.class);
        this.first = first;
        this.last = last;
    }

    /**
     * Private constructor that takes in a collection of the {@code Status} enum.
     * @param first - the users first name
     * @param last - the users last name
     * @param statusEnumSet - a set of statuses for the user
     */
    private EnumSetUser(String first, String last, Collection<Status> statusEnumSet) {
        this.statusEnumSet = EnumSet.copyOf(statusEnumSet);
        this.first = first;
        this.last = last;
    }

    /**
     * Add a status to the users bit field
     * @param user - an existing user object.  Users are treated as immutable - so a
     *             new user instance will be returned from this static method
     * @param status - a status to add to the enum set
     * @return a new user with an additional status compared to the one given as an argument
     */
    static EnumSetUser addStatus(EnumSetUser user, Status status) {

        EnumSet<Status> updatedStatuses;

        if (!user.statusEnumSet.isEmpty()) {
            updatedStatuses = EnumSet.copyOf(user.statusEnumSet);
            updatedStatuses.add(status);
        } else {
            updatedStatuses = EnumSet.of(status);
        }

        return new EnumSetUser(user.first, user.last, updatedStatuses);
    }

    /**
     * Remove a status from the users enum set
     * @param user - an existing user object.  Users are treated as immutable - so a
     *             new user instance will be returned from this static method
     * @param status - a status to remove from the enum set
     * @return a new user with one less status compared to the one given as an argument
     */
    static EnumSetUser removeStatus(EnumSetUser user, Status status) {

        EnumSet<Status> updatedStatuses = EnumSet.copyOf(user.statusEnumSet);
        updatedStatuses.remove(status);

        return new EnumSetUser(user.first, user.last, updatedStatuses);
    }

    /**
     * Perform an action (supplied by the second argument) when given a boolean
     * representing the existence of a status for the user.
     * @param status - the status to search for
     * @param action - an action to perform with the boolean result of whether the status
     *               exists for the user or not.  This argument is a {@code Consumer} functional
     *               interface implementation, which is a lambda function that passes one argument
     *               and returns nothing.
     */
    void containsStatus(Status status, Consumer<Boolean> action) {
        action.accept(containsStatus(status));
    }

    /**
     * Check for the existance of a status in a enum set
     * @param status - the status to search for
     * @return {@code true} if the status exists, {@code false} otherwise
     */
    boolean containsStatus(Status status) {
        return statusEnumSet.contains(status);
    }

    @Override
    public String toString() {
        return first + ", " + last + ", " + statusEnumSet.toString();
    }
}
