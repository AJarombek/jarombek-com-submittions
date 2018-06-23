/**
 * Main method to test the EnumSetUser class which uses an EnumSet
 * @author Andrew Jarombek
 * @since 6/22/2018
 */
public class EnumSetMain {

    public static void main(String... args) {

        // Base user has no fields in the enum set
        EnumSetUser user = new EnumSetUser("Andy", "Jarombek");

        // Add the Validated enum to the enum set
        EnumSetUser validatedUser = EnumSetUser.addStatus(user, EnumSetUser.Status.VALIDATED);

        // Add the Admin enum to the enum set
        EnumSetUser adminUser = EnumSetUser.addStatus(validatedUser, EnumSetUser.Status.ADMIN);

        // Remove the Admin enum from the enum set
        EnumSetUser revokedAdminUser = EnumSetUser.removeStatus(adminUser, EnumSetUser.Status.ADMIN);

        // Add the Subscribed enum to the enum set
        EnumSetUser subscribedUser = EnumSetUser.addStatus(revokedAdminUser, EnumSetUser.Status.SUBSCRIBED);

        System.out.println(user);
        System.out.println(validatedUser);
        System.out.println(adminUser);
        System.out.println(revokedAdminUser);
        System.out.println(subscribedUser);

        // Print out whether or not each enum exists for the user
        subscribedUser.containsStatus(EnumSetUser.Status.VALIDATED,
                (bool) -> System.out.println("User is Validated: " + bool));

        subscribedUser.containsStatus(EnumSetUser.Status.SUBSCRIBED,
                (bool) -> System.out.println("User is Subscribed: " + bool));

        subscribedUser.containsStatus(EnumSetUser.Status.ADMIN,
                (bool) -> System.out.println("User is Admin: " + bool));
    }
}
