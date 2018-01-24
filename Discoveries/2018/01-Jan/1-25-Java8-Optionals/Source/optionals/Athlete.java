package optionals;

import lombok.Data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;

/**
 * @author Andrew Jarombek
 * @since 1/23/2018
 */
@Data
public class Athlete {

    private String firstName;
    private String lastName;
    private Optional<Integer> age;
    private Optional<String> team;

    private ArrayList<Log> logs = new ArrayList<>();
    private HashMap<String, String> prs = new HashMap<>();

    private Required<String> requiredString = new Required<>();
    private Required<ArrayList<Log>> requiredArrayList = new Required<>();
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

    public boolean addPr(String event, String time) {

        return (Optional.ofNullable(event).isPresent() && Optional.ofNullable(time).isPresent() &&
                (Optional.ofNullable(this.prs.put(event, time)).isPresent() || Optional.ofNullable(this.prs.get(event)).isPresent()));
    }

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
