package exercise;

import java.time.Duration;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;

import static java.util.Comparator.comparing;

/**
 * A running exercise.  This class is not extendable - it has no public constructor.
 * @author Andrew Jarombek
 * @since 5/19/2018
 * @see Exercise
 */
public class Run implements Exercise, Comparable<Run> {

    /**
     * Java 8 Comparable construction method to be used in
     * the {@code compareTo()} method.
     */
    private static final Comparator<Run> COMPARATOR =
            comparing(Run::getDate)
                    .thenComparingDouble(Run::getMiles)
                    .thenComparing(Run::getTime)
                    .thenComparing(run -> run.surface);

    /**
     * {@code enum} representing the surface that the run was on.
     */
    public enum Surface {
        GRASS, SAND, ROAD, TRACK, TRAIL, UNKNOWN
    }

    /* Instance Variables */

    private final double miles;
    private final Duration time;
    private final LocalDate date;
    private final Surface surface;

    /* Private Constructor */

    private Run(double miles, Duration time, LocalDate date, Surface surface) {
        this.miles = Objects.requireNonNullElse(miles, 0.0);
        this.time = Objects.requireNonNullElse(time, Duration.ZERO);
        this.date = Objects.requireNonNullElse(date, LocalDate.now());
        this.surface = surface;
    }

    /* Static Factory Methods */

    public static Run create(double miles, Duration time, LocalDate date, Surface surface) {
        return new Run(miles, time, date, surface);
    }

    public static Run create(double miles, Duration time, LocalDate date) {
        return new Run(miles, time, date, Surface.UNKNOWN);
    }

    public static Run createNow(double miles, Duration time, Surface surface) {
        return new Run(miles, time, LocalDate.now(), surface);
    }

    public static Run createNow(double miles, Duration time) {
        return new Run(miles, time, LocalDate.now(), Surface.UNKNOWN);
    }

    /**
     * Find the hash code for this object
     * @return the hashed result
     * @see Object#hashCode()
     */
    @Override
    public int hashCode() {
        var result = date.hashCode();
        result = 31 * result + time.hashCode();
        result = 31 * result + Double.hashCode(miles);
        result = 31 * result + surface.hashCode();
        return result;
    }

    /**
     * Determine if another object is equal to this object
     * @param obj - another object to compare to
     * @return a {@code boolean} of whether the objects are equal
     * @see Object#equals(Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Run))
            return false;
        var exercise = (Run) obj;
        return Objects.equals(exercise.getDate(), getDate()) &&
                Objects.equals(exercise.getMiles(), getMiles()) &&
                Objects.equals(exercise.getTime(), getTime()) &&
                Objects.equals(exercise.getSurface(), getSurface());
    }

    /**
     * Generate a {@code String} representation of this object
     * @return {@code String} representation
     * @see Object#toString()
     */
    @Override
    public String toString() {
        String surface = (Optional.ofNullable(getSurface()).isPresent()) ?
                " - " + getSurface().toString() : "";

        return "(" + miles + " - " +
                time + " - " +
                date +
                surface + ")";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Run run) {
        return COMPARATOR.compare(this, run);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getMiles() {
        return miles;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Duration getTime() {
        return time;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public LocalDate getDate() {
        return date;
    }

    /**
     * Get the surface that the run was on.
     * @return the surface
     */
    public Optional<Surface> getSurface() {
        return Optional.ofNullable(surface);
    }
}
