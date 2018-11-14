import java.util.List;
import java.util.Optional;
import java.util.function.Function;

/**
 * @author Andrew Jarombek
 * @since 11/3/2018
 */
public class Composition {

    public static void main(String ...args) {

        // Create lambda functions to increment an integer and square an integer
        Function<Integer, Integer> inc = x -> x + 1;
        Function<Integer, Integer> sq = x -> x * x;

        // First increment an integer and then square it
        var res1 = inc.andThen(sq).apply(2);

        // Fist square an integer and then increment it
        var res2 = inc.compose(sq).apply(2);

        assert res1 == 9;
        assert res2 == 5;

        var list = List.of(1, 2);

        // Perform a function composition on a list using streams and the map reduce methods
        Optional<Integer> res3 = list.stream().map(x -> x + 1).reduce((x, y) -> x + y);

        assert res3.get() == 5;
    }
}
