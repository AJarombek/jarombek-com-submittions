/**
 * Writing function compositions in JavaScript
 * @author Andrew Jarombek
 * @since 11/3/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

const list = [1,2];
const list2 = ['My', 'name', 'is', 'Andy'];

/**
 * Using composition (method chaining), first increment each item in the list
 * and then add all the items together.
 * @param _ A list of numbers
 * @returns {*}
 */
const inc_sum = _ => _.map(x => x + 1) . reduce((acc, val) => acc + val);

assert(inc_sum(list) === 5);

/**
 * Using composition, add an emtpy space after each string in a list.  Then accumulate the list
 * and trim off the final empty space from the list.  There are much better ways to accomplish this task,
 * however this is another easy example of function compositions.
 * @param _ A list of strings
 * @returns {string}
 */
const list_str = _ => _.map(str => `${str} `) . reduce((acc, val) => acc + val) . trim();

assert(list_str(list2) === 'My name is Andy');