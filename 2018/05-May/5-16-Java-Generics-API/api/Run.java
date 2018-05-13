package api;

import java.time.Duration;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;

import static java.util.Comparator.comparing;

public class Run implements Exercise, Comparable<Run> {

    private static final Comparator<Run> COMPARATOR =
            comparing(Run::getDate)
                    .thenComparingDouble(Run::getMiles)
                    .thenComparing(Run::getTime)
                    .thenComparing(run -> run.surface);

    public enum Surface {
        GRASS, SAND, ROAD, TRACK, TRAIL
    }

    private double miles;
    private Duration time;
    private LocalDate date;
    private Surface surface;

    private Run(double miles, Duration time, LocalDate date, Surface surface) {
        this.miles = Objects.requireNonNullElse(miles, 0.0);
        this.time = Objects.requireNonNullElse(time, Duration.ZERO);
        this.date = Objects.requireNonNullElse(date, LocalDate.now());
        this.surface = surface;
    }

    public static Run create(double miles, Duration time, LocalDate date, Surface surface) {
        return new Run(miles, time, date, surface);
    }

    public static Run create(double miles, Duration time, LocalDate date) {
        return new Run(miles, time, date, null);
    }

    public static Run createNow(double miles, Duration time, Surface surface) {
        return new Run(miles, time, LocalDate.now(), surface);
    }

    public static Run createNow(double miles, Duration time) {
        return new Run(miles, time, LocalDate.now(), null);
    }

    @Override
    public int hashCode() {
        var result = date.hashCode();
        result = 31 * result + time.hashCode();
        result = 31 * result + Double.hashCode(miles);
        result = 31 * result + surface.hashCode();
        return result;
    }

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

    @Override
    public String toString() {
        String surface = (Optional.ofNullable(getSurface()).isPresent()) ?
                " - " + getSurface().toString() : "";

        return "(" + miles + " - " +
                time + " - " +
                date +
                surface + ")";
    }

    @Override
    public int compareTo(Run run) {
        return COMPARATOR.compare(this, run);
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

    public Optional<Surface> getSurface() {
        return Optional.ofNullable(surface);
    }
}
