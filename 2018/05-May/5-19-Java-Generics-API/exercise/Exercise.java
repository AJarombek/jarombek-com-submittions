package exercise;

import java.time.Duration;
import java.time.LocalDate;

/**
 * Interface to set up an Exercise
 * @author Andrew Jarombek
 * @since 5/19/2018
 * @see Run
 * @see Ski
 */
public interface Exercise {

    /**
     * Get the miles of the exercise.
     * @return number of miles
     */
    double getMiles();

    /**
     * Get the time spent on the exercise
     * @return time spent
     */
    Duration getTime();

    /**
     * Get the date of the exercise
     * @return the date
     */
    LocalDate getDate();

    /**
     * Default method that calculates the pace of the exercise based
     * on the time and miles.
     * @return the pace in a {@code Duration} instance
     */
    default Duration pace() {
        long secondsPerMile = getTime().toSeconds() / 60;
        return Duration.ofSeconds(secondsPerMile);
    }
}
