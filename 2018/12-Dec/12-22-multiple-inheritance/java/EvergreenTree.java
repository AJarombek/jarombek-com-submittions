/**
 * Interface for an evergreen tree
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

public interface EvergreenTree extends Tree {

    default String type() {
        return "Evergreen";
    }
}
