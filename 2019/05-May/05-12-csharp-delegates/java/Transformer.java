/**
 * Transformer functional interface which alters an integer value
 * @author Andrew Jarombek
 * @since 4/30/2019
 */

@FunctionalInterface
public interface Transformer {
    Integer transform(Integer x);
}
