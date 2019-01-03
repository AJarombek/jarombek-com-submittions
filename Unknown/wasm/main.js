/**
 * Main file for testing Web Assembly files
 * @author Andrew Jarombek
 * @since 12/31/2018
 */

const {execWat} = require('./wat-wasm');

// Basic addition operation in Web Assembly
execWat("wa/test.wat", (instance) => console.info(instance.exports.add(2, 2)));

execWat("wa/basics.wat", (instance) => {
    // Test calculating run paces with an integer as the distance
    console.info(instance.exports.calcPace1a(2, 12, 31));
    console.info(instance.exports.calcPace1b(2, 12, 31));

    // ... and then test it with a float as the distance
    console.info(instance.exports.calcPace2(2.6, 12, 31));
    console.info(instance.exports.calcPace2(2.2, 12, 31));

    // Get an integer at memory location 5.  It does not yet exist so 0 is returned
    console.info(instance.exports.getInt(5));

    // Assign the value 10 to memory location 5.
    console.info(instance.exports.setInt(5, 10));

    // Get an integer at memory location 5.  Now it exists so 10 is returned
    console.info(instance.exports.getInt(5));

    // Each of these function calls increments an integer in memory by one
    console.info(instance.exports.inc());
    console.info(instance.exports.inc());
    console.info(instance.exports.inc());

    // Perform division
    console.info(instance.exports.div(2, 2, 0));
    console.info(instance.exports.div(6, 2, 0));

    // Both floating point division and integer division result in the same value (because integer
    // division is truncated and the floating point division result is truncated to an integer)
    console.info(instance.exports.div(11, 3, 0));
    console.info(instance.exports.div(11, 3, 1));
});