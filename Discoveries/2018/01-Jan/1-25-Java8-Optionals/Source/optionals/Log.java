package optionals;

import lombok.Data;

import java.util.Optional;

/**
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
@Data
public class Log {

    private String title;
    private Optional<Double> miles;
    private Optional<String> time;
    private String date;
    private Optional<String> description;

    private Required<String> requiredString = new Required<>();

    public Log(String title, Double miles, String time, String date, String description) {
        this.title = title;
        this.miles = Optional.ofNullable(miles);
        this.time = Optional.ofNullable(time);
        this.date = Optional.ofNullable(date).orElse("2018-01-24");
        this.description = Optional.ofNullable(description);
    }

    public void setTitle(String title) {
        this.title = requiredString.error(title, "title");
    }

    public void setDate(String date) {
        this.date = requiredString.error(date, "date");
    }
}
