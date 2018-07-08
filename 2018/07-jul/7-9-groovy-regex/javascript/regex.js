/**
 * Working with Regular Expressions
 * @author Andrew Jarombek
 * @since 7/8/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

const datePattern = /\d{1,2}\/\d{1,2}\/\d{4}/;
const exactDatePattern = /^\d{1,2}\/\d{1,2}\/\d{4}$/;

const today = '7/8/2018';
const tomorrow = 'Tomorrow is 7/9/2018.';
const endOfYear = '12/31/2018';
const myBirthday = 'Feb. 26, 2018';

/* Exact matches */
assert(exactDatePattern.test(today));
assert(!exactDatePattern.test(tomorrow));
assert(exactDatePattern.test(endOfYear));
assert(!exactDatePattern.test(myBirthday));

/* Pattern existence match */
assert(datePattern.test(today));
assert(datePattern.test(tomorrow));
assert(datePattern.test(endOfYear));
assert(!datePattern.test(myBirthday));