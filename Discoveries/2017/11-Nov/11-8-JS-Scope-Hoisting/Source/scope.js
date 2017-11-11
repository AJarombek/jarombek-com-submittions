// Author: Andrew Jarombek
// Date: 11/8/2017
// Demonstrate scope and hoisting in JavaScript

/* 1. Hoisting in JavaScript */

console.info(x);  // Error??

var x = 10;

/* 2. You can define a scope with a {...} block.  The let declaration
constrains the variable to the inner scope. */

{
let firstName = "Andy";
  var lastName = "Jarombek";
  
  console.info(firstName); // Andrew
  console.info(lastName); // Jarombek
}

console.info(lastName); // Jarombek
console.info(firstName); // Reference Error

/* 3. The for loop leaks scope - so the variable j is available
in the global scope */

console.info(j); // undefined
console.info(i); // Reference Error

for (let i = 0; i < 10; i++) {
  console.info(i);
}

for (var j = 0; j < 10; j++) {
  console.info(j);
}

/*
console.info(j); // 10
console.info(i); // Reference Error
*/