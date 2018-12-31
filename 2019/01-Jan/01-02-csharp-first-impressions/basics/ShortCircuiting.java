/**
 * Prove that Java acts similar to C# with non-short circuiting boolean comparisons with bitwise operators.
 * @author Andrew Jarombek
 * @since 12/24/2018
 */

public class ShortCircuiting {

    public static void main(String... args) {

        var i = 0;

        // Short circuits, so i++ is never evaluated
        if (false && i++ == 1) {
            // Never reach this point
            assert false;
        }

        // Does not short circuit, so i++ is evaluated
        if (false & (i++ == 1)) {
            // Never reach this point
            assert false;
        }

        // Prove that identifier 'i' was incremented
        assert i == 1;
    }
}
