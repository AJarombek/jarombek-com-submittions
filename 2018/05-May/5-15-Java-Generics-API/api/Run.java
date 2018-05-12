package api;

import java.time.Duration;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;

import static java.util.Comparator.comparing;

public class Run implements Exercise, Comparable<Exercise> {

    private static final Comparator<Exercise> COMPARATOR =
            comparing(Exercise::getDate)
                    .thenComparing(Exercise::getDistance)
                    .thenComparing(Exercise::getDistance);

    public enum Surface {
        GRASS, SAND, ROAD, TRACK, TRAIL
    }

    private String distance;
    private Duration time;
    private LocalDate date;
    private Optional<Surface> surface;

    private Run(String distance, Duration time, LocalDate date, Surface surface) {
        this.distance = distance;
        this.time = time;
        this.date = date;
        this.surface = Optional.ofNullable(surface);
    }

    public static Run create(String distance, Duration time, LocalDate date, Surface surface) {
        return new Run(distance, time, date, surface);
    }

    public static Run create(String distance, Duration time, LocalDate date) {
        return new Run(distance, time, date, null);
    }

    public static Run createNow(String distance, Duration time, Surface surface) {
        return new Run(distance, time, LocalDate.now(), surface);
    }

    public static Run createNow(String distance, Duration time) {
        return new Run(distance, time, LocalDate.now(), null);
    }

    @Override
    public int hashCode() {
        var result = getDate().hashCode();
        result = 31 * result + getTime().hashCode();
        result = 31 * result + getDistance().hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Exercise))
            return false;
        var exercise = (Exercise) obj;
        return Objects.equals(exercise.getDate(), getDate()) &&
                Objects.equals(exercise.getDistance(), getDistance()) &&
                Objects.equals(exercise.getTime(), getTime());
    }

    @Override
    public String toString() {
        return getDistance() + " - " + getTime().toString() + " - " + getDate().toString();
    }

    @Override
    public int compareTo(Exercise ex) {
        return COMPARATOR.compare(this, ex);
    }

    /* Push Interface method implementations down to the child class */

    @Override
    public String getDistance() {
        return distance;
    }

    @Override
    public Duration getTime() {
        return time;
    }

    @Override
    public LocalDate getDate() {
        return date;
    }

    public Optional<Surface> getSurface() {
        return surface;
    }
}
