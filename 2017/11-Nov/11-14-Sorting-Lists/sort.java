import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import static java.util.stream.Collectors.toList;

/**
 * Create a person object to be used in a list.  Then sort this list using Java 8 Streams
 * @author Andrew Jarombek
 * @since 11/14/2017
 */
public class Person {

    private String first;
    private String last;

    public Person(String first, String last) {
        this.first = first;
        this.last = last;
    }

    @Override
    public String toString() {
        return "Person: { " + first + " " + last + " }";
    }

    public String getFirst() {
        return first;
    }

    public void setFirst(String first) {
        this.first = first;
    }

    public String getLast() {
        return last;
    }

    public void setLast(String last) {
        this.last = last;
    }

    public static void main(String[] args) {
        List<Person> list = Arrays.asList(new Person("Andrew", "Jar."), new Person("Thomas", "Cau."),
                new Person("Joe", "Smi."), new Person("Ben", "Fis."));
        System.out.println(Arrays.toString(list.toArray()));

        list = list.stream()
                .sorted(Comparator.comparing(Person::getLast))
                .collect(toList());
        System.out.println(Arrays.toString(list.toArray()));
    }
}