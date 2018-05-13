package api;

import java.time.Duration;
import java.time.LocalDate;

public interface Exercise {

    double getMiles();
    Duration getTime();
    LocalDate getDate();

    default Duration pace() {
        long secondsPerMile = getTime().toSeconds() / 60;
        return Duration.ofSeconds(secondsPerMile);
    }
}
