// Author: Andrew Jarombek
// Date: 11/15/2017
// Testing out features in strict mode

// Creates a new global variable 'hello' when not in strict mode
// Equivalent of 'var hello = "hi"'
hello = "hi";

// Setting properties to primitives silently fails when not in strict mode
false.not = "true";
console.info(false.not); // undefined

undefined = true
console.info(undefined); // undefined

console.info(hello); // hi

(function() {
  'use strict';
  
  try {
    undefined = "defined!";
  } catch(e) {
    console.error('Strict mode disallows assigning values to keywords');
    console.error(e);
  }
  
  // I am still allowed to set a keyword as the name of a variable
  let undefined = "defined!";
  
  var NaN = 1;
  
  console.info(undefined); // defined!
  console.info(NaN); // 1

  try {
    hey = "hello"
  } catch(e) {
    console.error('Strict mode disallows assigning to undeclared variables');
    console.error(e);
  }
  
  // Strict mode does not throw an error for duplicate property names
  var andy = {
    name: "Andy J",
    name: "whoops"
    /* Strict mode disallows duplicate parameter names for functions
    func: function(a, b, a) {
    console.info(a + b + c);
    }*/
  };
  
  console.info(andy.name); // whoops
  
  // Strict mode disallows setting properties for primitive values
  try {
    false.not = true;
  } catch(e) {
    console.error('Strict mode disallows setting properties for primitive values');
    console.error(e);
  } 
})();