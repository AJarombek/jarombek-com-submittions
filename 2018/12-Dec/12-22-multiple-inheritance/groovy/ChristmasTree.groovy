/**
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

trait ChristmasTree implements Tree {
    private String tree_type = "Christmas"

    def type() {
        return this.tree_type
    }
}