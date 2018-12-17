/**
 * Create a trait in groovy that represents a generic tree.  I use traits to demonstrate multiple inheritance.
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

trait Tree {
    int height_feet
    int height_inches

    /**
     * The height of the tree in feet and inches
     * @return A string of the form ('feet', 'inches')
     */
    def height() {
        return "(${this.height_feet}, ${this.height_inches})"
    }
}