package optionals;

import java.util.Optional;

/**
 * Test out the null safe Athlete API with Optionals
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
public class Main {

    public static void main(String... args) {
        Athlete athlete = new Athlete("andy", "jarombek");

        athlete.setAge(Optional.of(22));

        // athlete.setFirstName(null); - Unable to Set Required Field 'firstName' to null
        athlete.setFirstName("andrew");

        // Check to see if adding PR's are successful
        boolean validPR = athlete.addPr("5000m", "15:27");
        boolean invalidPR = athlete.addPr(null, "15:27");

        System.out.println("Valid PR Succeeded: " + validPR);
        System.out.println("Invalid PR Succeeded: " + invalidPR);

        // Safely try and get a value from a map with Optionals
        Optional<String> value = Optional.ofNullable(athlete.getPrs().get("5000m"));
        value.ifPresent(System.out::println);

        System.out.println(athlete);

        Athlete noNameAthlete = new Athlete(null, null);
        System.out.println(noNameAthlete);
    }
}
