package optionals;

import lombok.AccessLevel;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

import java.util.Optional;

/**
 * API for a running log
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
@Data
public class Log {

    // Instance variables exposed via getters and setters for the API
    private String title;
    private Optional<Double> miles;
    private Optional<String> time;
    private String date;
    private Optional<String> description;

    // Instance variable private to the API for internal use
    @Getter(AccessLevel.NONE)
    @Setter(AccessLevel.NONE)
    private Required<String> requiredString = new Required<>();

    public Log(String title, Double miles, String time, String date, String description) {
        this.title = title;
        this.miles = Optional.ofNullable(miles);
        this.time = Optional.ofNullable(time);
        this.date = Optional.ofNullable(date).orElse("2018-01-24");
        this.description = Optional.ofNullable(description);
    }

    // Define custom setters for required variables - throw an error if the user passes null to the setter

    public void setTitle(String title) {
        this.title = requiredString.error(title, "title");
    }

    public void setDate(String date) {
        this.date = requiredString.error(date, "date");
    }
}
