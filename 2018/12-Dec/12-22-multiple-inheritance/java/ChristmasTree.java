/**
 * Interface for a christmas tree
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

public interface ChristmasTree extends Tree {

    default String type() {
        return "Christmas";
    }
}
