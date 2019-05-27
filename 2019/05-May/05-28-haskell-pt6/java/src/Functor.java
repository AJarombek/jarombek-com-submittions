import java.util.function.Function;

/**
 * Interface representing a Functor.
 * @author Andrew Jarombek
 * @since 5/25/2019
 */

public interface Functor<T> {

    /**
     * Map a function to values wrapped inside the Functor instance
     * @param f A function that is applied to the Functor
     * @param <R> The generic type that will be wrapped in the Functor return value
     * @return A new instance of Functor
     */
    <R> Functor<R> fmap(Function<T, R> f);
}
