package api;

import java.time.Duration;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.Objects;

import static java.util.Comparator.comparing;

public class Ski implements Exercise, Comparable<Ski> {

    private int hashCode;

    private static final Comparator<Ski> COMPARATOR =
            comparing(Ski::getDate)
                    .thenComparingDouble(Ski::getMiles)
                    .thenComparing(Ski::getTime)
                    .thenComparing(ski -> ski.type);

    public enum Type {
        Nordic, Downhill
    }

    private double miles;
    private Duration time;
    private LocalDate date;
    private Ski.Type type;

    private Ski(double miles, Duration time, LocalDate date, Ski.Type type) {
        this.miles = Objects.requireNonNullElse(miles, 0.0);
        this.time = Objects.requireNonNullElse(time, Duration.ZERO);
        this.date = Objects.requireNonNullElse(date, LocalDate.now());
        this.type = Objects.requireNonNullElse(type, Type.Nordic);
    }

    public static Ski create(double miles, Duration time, LocalDate date, Ski.Type type) {
        return new Ski(miles, time, date, type);
    }

    public static Ski createNow(double miles, Duration time, Ski.Type type) {
        return new Ski(miles, time, LocalDate.now(), type);
    }

    @Override
    public int hashCode() {
        var result = hashCode;

        if (result == 0) {
            result = date.hashCode();
            result = 31 * result + time.hashCode();
            result = 31 * result + Double.hashCode(miles);
            result = 31 * result + type.hashCode();
        }

        return result;
    }

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

    @Override
    public String toString() {
        return "(" + getMiles() + " - " +
                getTime().toString() + " - " +
                getDate().toString() + " - " +
                getType().toString() + ")";
    }

    @Override
    public int compareTo(Ski ski) {
        return COMPARATOR.compare(this, ski);
    }

    @Override
    public double getMiles() {
        return miles;
    }

    @Override
    public Duration getTime() {
        return time;
    }

    @Override
    public LocalDate getDate() {
        return date;
    }

    public Ski.Type getType() {
        return type;
    }
}
