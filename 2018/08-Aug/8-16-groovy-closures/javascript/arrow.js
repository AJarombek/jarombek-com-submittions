/**
 * Comparing JavaScript arrow functions to Groovy closures
 * @author Andrew Jarombek
 * @since 8/15/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

let number = 1;

// JavaScript arrow functions can access variables in their defined scope.
const timesTen = () => number *= 10;

assert(number === 1);
timesTen();
assert(number === 10);