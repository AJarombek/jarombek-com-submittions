import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import static java.util.stream.Collectors.toList;

/**
 * Functor implementation of a list.
 * @author Andrew Jarombek
 * @since 5/25/2019
 */

public class FList<T> implements Functor<T, FList<?>> {

    private List<T> list;

    FList(Iterable<T> iterable) {
        iterable.forEach(list::add);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <R> FList<?> fmap(Function<T, R> f) {
        List<R> newList = list.stream().map(f).collect(toList());
        return new FList<R>(newList);
    }
}
