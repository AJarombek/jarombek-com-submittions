/**
 * Demonstrate how equality between types works in TypeScript and how it compares to JavaScript.
 * @author Andrew Jarombek
 * @since 8/9/2019
 */
var assert = function (assertion) {
    console.assert(assertion, "Assertion failed!");
};
// This code is valid in JavaScript but won't compile in TypeScript.
// TS2365: Operator == cant be applied to types '2' and "2".
// assert(2 == "2");
// assert(2 !== "2"); 
