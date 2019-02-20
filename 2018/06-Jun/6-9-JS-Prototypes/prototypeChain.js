/**
 * Observe different prototype chains in JavaScript
 * @author Andrew Jarombek
 * @since 6/7/2018
 */

/* Object Prototype */

const person = {
    name: "Andrew Jarombek",
    age: 23
};

// Traverse the prototype chain of an object
// person{name, age} -> Object.prototype -> null
const objPrototype = Object.getPrototypeOf(person);
const objProtoPrototype = Object.getPrototypeOf(objPrototype);

console.info(objPrototype);
console.info(objProtoPrototype);

/* Array Prototype */

const array = [1,2,3,4];

// Traverse the prototype chain of an array
// [1,2,3,4] -> Array.prototype -> Object.prototype -> null
const arrayPrototype = Object.getPrototypeOf(array);
const arrayProtoPrototype = Object.getPrototypeOf(arrayPrototype);

console.info(arrayPrototype);
console.info(arrayProtoPrototype);

/* Function Prototype */

const addition = (x, y) => x + y;

// Traverse the prototype chain of a function
// {f(x, y)} -> Function.prototype -> Object.prototype -> null
const funcPrototype = Object.getPrototypeOf(addition);
const funcProtoPrototype = Object.getPrototypeOf(funcPrototype);

console.info(funcPrototype);
console.info(funcProtoPrototype);