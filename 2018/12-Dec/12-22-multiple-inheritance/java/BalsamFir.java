import java.util.List;

/**
 * Demonstrate the closest thing to multiple inheritance in Java: implementing multiple interfaces
 * with default methods.
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

public class BalsamFir implements EvergreenTree, ChristmasTree {

    private List<Integer> height;

    public BalsamFir(int feet, int inches) {
        this.height = List.of(feet, inches);
    }

    @Override
    public String type() {
        return EvergreenTree.super.type();
    }

    @Override
    public String height() {
        return "(" + height.get(0) + ", " + height.get(1) + ")";
    }

    /**
     * Determine if the leaves persist in winter or not.
     * @return {@code true}
     */
    public boolean leafPersistence() {
        return EvergreenTree.super.type().equals("Evergreen");
    }

    /**
     * Determine if the tree is a christmas tree.
     * @return {@code true}
     */
    public boolean isChristmasTree() {
        return ChristmasTree.super.type().equals("Christmas");
    }
}
