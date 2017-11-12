// Author: Andrew Jarombek
// Date: 11/11/2017
// Demonstrate the uses of 'this' in JavaScript

/* this won't bind to the global object under strict mode */

(function() {
  'use strict';
  
  console.info(this);
})();

/* You can explicitly set 'this' with the call(), apply(), and bind() functions */

function name() {
  console.info(this.username);
}

var user = {
  username: "Andy"
}

name(); // undefined
name.call(user); // Andy

var n = name.bind(user); // Andy

var secondUser = {
  username: "Joe"
}

n.call(secondUser);
n.bind(secondUser);
n(); // Andy

/* The equivalent of bind() */

var n = function _n() {
  name.call(user);
}

/* If you call a function with an object context, that object is 'this' */

var user = {
  username: "Andy",
    
  name: function name() {
    console.info(this.username);
  }
};

user.name(); // Andy

user.name.call({username: "Joe"}); // Joe