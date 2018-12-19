/**
 * Experiment with mixins
 * Source: ["https://bit.ly/1RhbaYR"]
 * @author Andrew Jarombek
 * @since 12/18/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

const tree = {
    getHeight: () => this._height
};

const evergreenTree = {
    type: () => "Evergreen",
    leafPersistence: true
};

const christmasTree = {
    type: () => "Christmas"
};

const balsamFir = (height) => {
    this._height = height;

    // Create a composition of 'this' and the tree, evergreenTree, and christmasTree mixins
    return Object.assign(this, { ...tree, ...evergreenTree, ...christmasTree });
};

const balsam = balsamFir(7);

assert(balsam.getHeight() === 7);
assert(balsam.type() === "Christmas");