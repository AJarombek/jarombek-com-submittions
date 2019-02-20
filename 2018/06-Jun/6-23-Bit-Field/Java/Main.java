/**
 * Main method to test the User class which uses a bit field
 * @author Andrew Jarombek
 * @since 6/22/2018
 */
public class Main {

    public static void main(String... args) {

        // Base user has a bit field of value 000
        User user = new User("Andy", "Jarombek");

        // Add the validated flag to the bit field: 000 -> 001
        User validatedUser = User.addStatus(user, User.VALIDATED);

        // Add the admin flag to the bit field: 001 -> 101
        User adminUser = User.addStatus(validatedUser, User.ADMIN);

        // Remove the admin flag from the bit field: 101 -> 001
        User revokedAdminUser = User.removeStatus(adminUser, User.ADMIN);

        // Add the subscribed flag to the bit field: 001 -> 011
        User subscribedUser = User.addStatus(revokedAdminUser, User.SUBSCRIBED);

        System.out.println(user);
        System.out.println(validatedUser);
        System.out.println(adminUser);
        System.out.println(revokedAdminUser);
        System.out.println(subscribedUser);

        // Check to see if the user has different statuses based on the bit field
        subscribedUser.containsStatus(User.VALIDATED,
                (bool) -> System.out.println("User is Validated: " + bool));

        subscribedUser.containsStatus(User.SUBSCRIBED,
                (bool) -> System.out.println("User is Subscribed: " + bool));

        subscribedUser.containsStatus(User.ADMIN,
                (bool) -> System.out.println("User is Admin: " + bool));
    }
}
