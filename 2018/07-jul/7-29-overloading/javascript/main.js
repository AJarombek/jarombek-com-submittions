const Animal = require("./Animal");
const Plant = require("./Plant");
const Garden = require("./Garden");

/**
 * Create a garden to demonstrate method overloading in JavaScript
 * @author Andrew Jarombek
 * @since 7/27/2018
 */

/**
 * Test a certain case for JavaScript truth.  Assertions are a nice form of inline documentation
 * @param assertion - a statement that will be checked whether or not it is truthy
 */
const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

const msGroundhog = new Animal("Ms. Groundhog", Animal.Species.GROUNDHOG,
    "Enjoys lounging and eating grass all day");
const mrGroundhog = new Animal("Mr. Groundhog", Animal.Species.GROUNDHOG);

const doe = new Animal("doe", Animal.Species.DEER,
    "from the tip of his wand burst the silver doe");
const bunny = new Animal("bunny", Animal.Species.RABBIT);

const hosta = new Plant(Plant.Species.HOSTA);
const lily = new Plant(Plant.Species.DAYLILY, true);
const iris = new Plant(Plant.Species.IRIS, false);

const garden = new Garden([msGroundhog, mrGroundhog, doe, bunny], [hosta, lily, iris]);

// Both checks on animals and plants will succeed
assert(garden.inGarden(doe));
assert(garden.inGarden(lily));

assert(!garden.inGarden(new Animal("momma rabbit", Animal.Species.RABBIT)));