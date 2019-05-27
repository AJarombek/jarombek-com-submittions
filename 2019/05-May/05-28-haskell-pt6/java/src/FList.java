import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import static java.util.stream.Collectors.toList;

/**
 * Functor implementation of a list.
 * @author Andrew Jarombek
 * @since 5/25/2019
 */

public class FList<T> implements Functor<T> {

    // The FList class uses composition to hold an instance of a List object
    private List<T> list;

    /**
     * Construct an FList object with any iterable collection.  Converts the Iterable to a List.
     * @param iterable - Iterable collection used as the contents of the FList
     */
    public FList(Iterable<T> iterable) {
        list = new ArrayList<>();
        iterable.forEach(list::add);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <R> FList<R> fmap(Function<T, R> f) {
        List<R> newList = list.stream().map(f).collect(toList());
        return new FList<>(newList);
    }

    /**
     * Retrieve the internal list object
     * @return The Generic list
     */
    public List<T> getList() {
        return list;
    }
}
