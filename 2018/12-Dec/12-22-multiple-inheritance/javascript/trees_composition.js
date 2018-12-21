/**
 * Experiment with object compositions
 * @author Andrew Jarombek
 * @since 12/20/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

/**
 * Base tree functionality - every tree should have a height
 * @type {{getHeight: function(): *}}
 */
const tree = {
    getHeight: () => this._height
};

/**
 * Evergreen tree functionality
 * @type {{type: function(): string, leafPersistence: boolean}}
 */
const evergreenTree = {
    type: () => "Evergreen",
    leafPersistence: true
};

/**
 * Christmas tree functionality
 * @type {{type: function(): string}}
 */
const christmasTree = {
    type: () => "Christmas"
};

/**
 * A balsam fir tree.  Use object composition with the tree, evergreen tree,
 * and christmas tree objects
 * @param height - how tall the tree is
 * @return {*}
 */
const balsamFir = (height) => {

    // Object composition
    this.tree = tree;
    this.evergreenTree = evergreenTree;
    this.christmasTree = christmasTree;

    this._height = height;

    // Delegate functions and properties to composed objects
    this.type = christmasTree.type;
    this.leafPersistence = evergreenTree.leafPersistence;
    this.getHeight = tree.getHeight;

    return this;
};

const balsam = balsamFir(7);

assert(balsam.getHeight() === 7);
assert(balsam.type() === "Christmas");
assert(balsam.leafPersistence);