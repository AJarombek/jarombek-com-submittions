/**
 * Demonstrate how equality between types works in TypeScript and how it compares to JavaScript.
 * Sources: [https://basarat.gitbooks.io/typescript/content/docs/javascript/equality.html]
 * @author Andrew Jarombek
 * @since 8/9/2019
 */
import {EqualityHelper} from "./EqualityHelper";

const assert = (assertion: any) => {
    console.assert(assertion, `Assertion failed!`);
};

// This code is valid in JavaScript but won't compile in TypeScript.  TypeScript doesn't allow for
// values of guaranteed different types to be compared with == or ===.
// TS2365: Operator == cant be applied to types '2' and "2".
// assert(2 == "2");
// assert(2 !== "2");

// As expected, comparing two primitive type values is permitted and behaves as expected.
assert("hello" == "hello");
assert("hello" === "hello");

// Since TypeScript compiles to JavaScript, we can trick TypeScript about the compile-time types of
// mismatching types.  This allows for == and === to compile for unequal types.

// == works the same in TypeScript as in JavaScript if the compile time type of a string or number
// being compared is 'any' or 'Object'.
const age: number = 24;
const ageStr: any = "24";

// This assertion compiles and succeeds thanks to JavaScript type coercion.
assert(age == ageStr);
assert(age !== ageStr);

const ageObj: Object = "24";
assert(age == ageObj);

// Just like JavaScript, objects are tested for reference equality with == and ===.  Use the same
// JavaScript functions to test for object equality.
assert({} != {});

// TypeScript class representing wrapping paper for presents.  It creates objects with two
// properties - brand and pattern.
class WrappingPaper {
    constructor(public brand: string, public pattern: string) {}
}

// Create a new object instance of WrappingPaper and assign its reference to another variable.
const blanketWrappingPaper: WrappingPaper = new WrappingPaper("hallmark", "disney princess");
const blanketWrappingPaper2: WrappingPaper = blanketWrappingPaper;

// Create a new object with wrapping paper as its prototype.  Then set its brand and pattern
// properties to the values found on the prototype.
const blanketWrappingPaper3: WrappingPaper = Object.create(blanketWrappingPaper);
blanketWrappingPaper3.brand = Object.getPrototypeOf(blanketWrappingPaper3).brand;
blanketWrappingPaper3.pattern = Object.getPrototypeOf(blanketWrappingPaper3).pattern;

// Prove that == tests for reference equality.
assert(blanketWrappingPaper == blanketWrappingPaper2);
assert(blanketWrappingPaper != blanketWrappingPaper3);

// Prove that the custom equals() method checks for value equality of object properties.
assert(EqualityHelper.equals(blanketWrappingPaper, blanketWrappingPaper2));
assert(EqualityHelper.equals(blanketWrappingPaper, blanketWrappingPaper3));