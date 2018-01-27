package optionals;

import lombok.AccessLevel;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;

/**
 * API for an athlete
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
@Data
public class Athlete {

    // Instance variables exposed via getters and setters for the API
    private String firstName;
    private String lastName;
    private Optional<Integer> age;
    private Optional<String> team;

    private ArrayList<Log> logs = new ArrayList<>();
    private HashMap<String, String> prs = new HashMap<>();

    // Instance variables private to the API for internal use
    @Getter(AccessLevel.NONE)
    @Setter(AccessLevel.NONE)
    private Required<String> requiredString = new Required<>();

    @Getter(AccessLevel.NONE)
    @Setter(AccessLevel.NONE)
    private Required<ArrayList<Log>> requiredArrayList = new Required<>();

    @Getter(AccessLevel.NONE)
    @Setter(AccessLevel.NONE)
    private Required<HashMap<String, String>> requiredHashMap = new Required<>();

    public Athlete(String firstName, String lastName) {
        this.firstName = Optional.ofNullable(firstName).orElse("Foo");
        this.lastName = Optional.ofNullable(lastName).orElse("Bar");
    }

    public Athlete(String firstName, String lastName, Integer age) {
        this.firstName = Optional.ofNullable(firstName).orElse("Foo");
        this.lastName = Optional.ofNullable(lastName).orElse("Bar");
        this.age = Optional.ofNullable(age);
    }

    /**
     * Add a new PR to the hash map for this athlete
     * @param event - the event name
     * @param time - the time of the PR
     * @return whether or not the PR was added
     */
    public boolean addPr(String event, String time) {

        // Last get() is necessary because put() returns null if the keys previous value was null
        return (Optional.ofNullable(event).isPresent() && Optional.ofNullable(time).isPresent() &&
                (Optional.ofNullable(this.prs.put(event, time)).isPresent() || Optional.ofNullable(this.prs.get(event)).isPresent()));
    }

    // Define custom setters for required variables - throw an error if the user passes null to the setter

    public void setFirstName(String firstName) {
        this.firstName = requiredString.error(firstName, "firstName");
    }

    public void setLastName(String lastName) {
        this.firstName = requiredString.error(lastName, "lastName");
    }

    public void setLogs(ArrayList<Log> logs) {
        this.logs = requiredArrayList.error(logs, "logs");
    }

    public void setPrs(HashMap<String, String> prs) {
        this.prs = requiredHashMap.error(prs, "prs");
    }
}
