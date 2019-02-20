/**
 * Experiment with mixins
 * Source: ["https://bit.ly/1RhbaYR"]
 * @author Andrew Jarombek
 * @since 12/18/2018
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
 * A balsam fir tree.  Mix-in the tree, evergreen tree, and christmas tree objects
 * @param height - how tall the tree is
 * @return {*}
 */
const balsamFir = (height) => {
    this._height = height;

    // Create a composition of 'this' and the tree, evergreenTree, and christmasTree mixins.
    // Properties from the last mixed in object replace duplicates from prior objects
    return Object.assign(this, { ...tree, ...evergreenTree, ...christmasTree });
};

const balsam = balsamFir(7);

assert(balsam.getHeight() === 7);
assert(balsam.type() === "Christmas");
assert(balsam.leafPersistence);