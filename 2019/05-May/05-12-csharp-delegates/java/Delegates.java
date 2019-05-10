import java.util.ArrayList;
import java.util.List;

/**
 * Compare a Java implementation of the C# delegate pattern
 * @author Andrew Jarombek
 * @since 4/30/2019
 */

public class Delegates {

    /**
     * Performs a transformation on every element of the list.  Keeps the original list in-tact, returns a new
     * list instance.
     * @param list A list of integers
     * @param transformer A transformation functional interface which is given a function at runtime
     * @return A new list instance with newly mapped values
     */
    private static List<Integer> map(List<Integer> list, Transformer transformer) {
        List<Integer> newList = new ArrayList<>(list);
        newList.replaceAll(transformer::transform);
        return newList;
    }

    public static void main(String ...args) {
        Transformer transformer = x -> x * 2;
        var list = List.of(5, 10, 15, 20, 25, 31);

        var newList = map(list, transformer);

        // Prove that the new list is mapped to new values
        assert newList.get(0) == 10;
        assert newList.get(5) == 62;

        // Prove that the original list wasn't mutated
        assert list.get(0) == 5;
        assert list.get(5) == 31;
    }
}
