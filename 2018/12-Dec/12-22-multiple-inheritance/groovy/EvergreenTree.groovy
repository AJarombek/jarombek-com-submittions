/**
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

trait EvergreenTree implements Tree {
    private String tree_type = "Evergreen"

    def type() {
        return this.tree_type
    }
}