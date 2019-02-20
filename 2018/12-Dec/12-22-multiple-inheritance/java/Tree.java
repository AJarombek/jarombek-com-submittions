/**
 * Interface for a generic tree
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

public interface Tree {

    String type();

    default String height() {
        return "(0, 0)";
    }
}
