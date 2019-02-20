import java.util.List;
import static java.util.stream.Collectors.toList;

/**
 * Demonstrate basic lambda functionality compared to Groovy
 * @author Andrew Jarombek
 * @since 8/15/2018
 */
public class Lambdas {

    public static void main(String... args) {

        var list = List.of(1,2,3,4,5);

        System.out.println(list); // [1,2,3,4,5]

        list = list.stream().map(item -> item * 2).collect(toList());

        System.out.println(list); // [2,4,6,8,10]

        // Will not compile - variables used in a lambda must be final or effectively final

        /*
        int listTotal = 0;
        list.forEach(item -> {
            listTotal += item;
        });
        */
    }
}