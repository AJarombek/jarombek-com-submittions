import java.util.Objects;

/**
 * Object representing a ball of yarn for knitting.
 * @author Andrew Jarombek
 * @since 8/17/2019
 */

public class Yarn {

    // Instance variables
    private String fiber;
    private String color;
    private int yards;

    /**
     * Private constructor for a new ball of yarn.  Can only be invoked by the static factory method.
     * @param fiber The fiber that the yarn is made of.
     * @param color The visual color of the yarn.
     * @param yards The length of the yarn in yards
     */
    private Yarn(String fiber, String color, int yards) {
        this.fiber = fiber;
        this.color = color;
        this.yards = yards;
    }

    /**
     * Static factory method for constructing a new ball of yarn.  Requires that a value be assigned for each
     * instance variable, although it does accept null values.
     * @param fiber The fiber that the yarn is made of.
     * @param color The visual color of the yarn.
     * @param yards The length of the yarn in yards.
     * @return A new Yarn object.
     */
    static Yarn create(String fiber, String color, int yards) {
        return new Yarn(fiber, color, yards);
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Yarn))
            return false;
        var otherYarn = (Yarn) obj;
        return Objects.equals(fiber, otherYarn.fiber)
                && Objects.equals(color, otherYarn.color)
                && yards == otherYarn.yards;
    }
}
