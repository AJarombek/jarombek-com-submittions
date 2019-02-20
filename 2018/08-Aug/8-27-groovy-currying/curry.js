/**
 * Writing curried functions and partial applications in JavaScript
 * @author Andrew Jarombek
 * @since 11/3/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

/**
 * A curried function that takes in two numerical arguments and multiplies them.  With this
 * curried function it is easy to create partial applications.
 * @param n - an integer that can be supplied to create a partial application
 * @returns {function(*): number}
 */
const mult = n => m => n * m;

// Partial application for multiplying a number by 2
const x2 = mult(2);

// Partial application for multiplying a number by 10
const x10 = mult(10);

assert(x2(3) === 6);
assert(x10(3) === 30);

// Calling the curried function without using a partial application
const res = mult(2)(5);

assert(res === 10);