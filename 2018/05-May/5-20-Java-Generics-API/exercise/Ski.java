package exercise;

import java.time.Duration;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.Objects;

import static java.util.Comparator.comparing;

/**
 * A skiing exercise.  This class is not extendable - it has no public constructor.
 * @author Andrew Jarombek
 * @since 5/19/2018
 * @see Exercise
 */
public class Ski implements Exercise, Comparable<Ski> {

    /**
     * Cache the result of the {@code hashCode()} method
     */
    private int hashCode;

    /**
     * Java 8 Comparable construction method to be used in
     * the {@code compareTo()} method.
     */
    private static final Comparator<Ski> COMPARATOR =
            comparing(Ski::getDate)
                    .thenComparingDouble(Ski::getMiles)
                    .thenComparing(Ski::getTime)
                    .thenComparing(ski -> ski.type);

    /**
     * {@code enum} representing the type of skiing.
     */
    public enum Type {
        Nordic, Downhill, Unknown
    }

    /* Instance Variables */

    private final double miles;
    private final Duration time;
    private final LocalDate date;
    private final Ski.Type type;

    /* Private Constructor */

    private Ski(double miles, Duration time, LocalDate date, Ski.Type type) {
        this.miles = Objects.requireNonNullElse(miles, 0.0);
        this.time = Objects.requireNonNullElse(time, Duration.ZERO);
        this.date = Objects.requireNonNullElse(date, LocalDate.now());
        this.type = Objects.requireNonNullElse(type, Type.Unknown);
    }

    /* Static Factory Methods */

    public static Ski create(double miles, Duration time, LocalDate date, Ski.Type type) {
        return new Ski(miles, time, date, type);
    }

    public static Ski createNow(double miles, Duration time, Ski.Type type) {
        return new Ski(miles, time, LocalDate.now(), type);
    }

    /**
     * Find the hash code for this object
     * @return the hashed result
     * @see Object#hashCode()
     */
    @Override
    public int hashCode() {
        var result = hashCode;

        if (result == 0) {
            result = date.hashCode();
            result = 31 * result + time.hashCode();
            result = 31 * result + Double.hashCode(miles);
            result = 31 * result + type.hashCode();
            hashCode = result;
        }

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
        if (!(obj instanceof Ski))
            return false;
        var ski = (Ski) obj;
        return Objects.equals(ski.getDate(), getDate()) &&
                Objects.equals(ski.getMiles(), getMiles()) &&
                Objects.equals(ski.getTime(), getTime()) &&
                Objects.equals(ski.getType(), getType());
    }

    /**
     * Generate a {@code String} representation of this object
     * @return {@code String} representation
     * @see Object#toString()
     */
    @Override
    public String toString() {
        return "(" + getMiles() + " - " +
                getTime().toString() + " - " +
                getDate().toString() + " - " +
                getType().toString() + ")";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Ski ski) {
        return COMPARATOR.compare(this, ski);
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
     * The type of skiing performed
     * @return type of skiing
     */
    public Ski.Type getType() {
        return type;
    }
}
