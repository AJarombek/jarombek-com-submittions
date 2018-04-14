// Author: Andrew Jarombek
// Date: 11/14/2017
// Sorting a List in JavaScript with a callback function

let people = [
  {first: "Andrew", last: "Jar."},
  {first: "Thomas", last: "Cau."},
  {first: "Joe", last: "Smi."},
  {first: "Ben", last: "Fis."}
];

// If I call console.info here I may get the original array or the
// sorted array.  This is because console functions are not standardized
// and may run asyncronously!  You can't always trust them.
console.info(people);

people.sort(function(a, b) {
    return a.last > b.last ? 1 : 0;
});

console.info(people);