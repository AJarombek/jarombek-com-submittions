import java.util.List;
import java.util.function.Function;

/**
 * Functor implementation of a pair of values.
 * @author Andrew Jarombek
 * @since 5/25/2019
 */

public class FPair<T> implements Functor<T> {

    // The FPair object contains two values of matching types
    private T item1;
    private T item2;

    /**
     * Construct a pair based on two values of the same type.
     * @param item1 The first value
     * @param item2 The second value
     */
    public FPair(T item1, T item2) {
        this.item1 = item1;
        this.item2 = item2;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <R> FPair<R> fmap(Function<T, R> f) {
        return new FPair<>(f.apply(item1), f.apply(item2));
    }

    /**
     * Get the internal objects and return them as a list
     * @return A list containing two objects
     */
    public List<T> getPair() {
        return List.of(item1, item2);
    }
}
