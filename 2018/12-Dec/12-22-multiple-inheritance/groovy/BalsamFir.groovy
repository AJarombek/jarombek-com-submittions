/**
 * Multiple inheritance of traits in Groovy
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

class BalsamFir implements ChristmasTree, EvergreenTree {

    BalsamFir(heightMap) {
        this.height_feet = heightMap.feet
        this.height_inches = heightMap.inches
    }

    /**
     * Determine if the leaves persist in winter or not.  Method {@code type()} on the {@link EvergreenTree} trait
     * is used here because it is declared after {@link ChristmasTree} in the {@code implements} clause.
     * @return {@code true}
     */
    def leafPersistence() {
        return this.type() == "Evergreen"
    }

    /**
     * Determine if the tree is a christmas tree.  Explicitly use the {@code type()} method from
     * the implemented {@link ChristmasTree} trait.
     * @return {@code true}
     */
    def isChristmasTree() {
        return ChristmasTree.super.type() == "Christmas"
    }
}
