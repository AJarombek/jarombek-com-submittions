package api;

import java.time.Duration;
import java.time.LocalDate;

public interface Exercise {

    public String getDistance();
    public Duration getTime();
    public LocalDate getDate();
}
