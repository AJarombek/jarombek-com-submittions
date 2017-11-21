// Author: Andrew Jarombek
// Date: 11/20/2017
// Demonstrate arrow functions in JavaScript ES6

'use strict';

function mult() {
this.x = 50;
  
  var x2Arrow = () => { 
    console.info(this); 
    return this.x * 2
  };
  
  var x2 = function times2() { 
    console.info(this); 
    return this.x * 2
  };
  
  return {x2, x2Arrow};
}

// this is assigned to the newly constructed function scope
var m = new mult();

console.info(m.x2Arrow()); // this -> mult {x: 50}, return -> 100
console.info(m.x2()); // this -> {x2: function(), x2Arrow: function()}, return -> NaN

console.info(m.x2Arrow.call({x: 5})); // this -> mult {x: 50}, return -> 100
console.info(m.x2.call({x: 5})); // this => {x: 5}, return -> 10