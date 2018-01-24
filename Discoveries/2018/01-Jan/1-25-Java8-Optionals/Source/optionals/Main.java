package optionals;

import java.util.Optional;

/**
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
public class Main {

    public static void main(String... args) {
        Athlete athlete = new Athlete("andy", "jarombek");

        athlete.setAge(Optional.of(22));

        // athlete.setFirstName(null); - Unable to Set Required Field 'firstName' to null
        athlete.setFirstName("andrew");

        boolean validPR = athlete.addPr("5000m", "15:27");
        boolean invalidPR = athlete.addPr(null, "15:27");

        System.out.println("Valid PR Succeeded: " + validPR);
        System.out.println("Invalid PR Succeeded: " + invalidPR);

        System.out.println(athlete);

        Athlete noNameAthlete = new Athlete(null, null);
        System.out.println(noNameAthlete);
    }
}
