// Author: Andrew Jarombek
// Date: 12/12/2017
// Show captures in regular expressions

let date = "02/26/1995";

// Grouping with () pattern also creates a capture of the groupings contents 
let pattern = /(\d{1,2})\/(\d{2})\/(\d{4})/;
let captures = date.match(pattern);
console.info(captures);

// The captures can be accessed from the array produced by match()
console.info(`Month: ${captures[1]}, Day: ${captures[2]}, Year: ${captures[3]}`);

// Remove capturing if it is not needed
let pattern2 = /(?:\d{1,2})\/(?:\d{2})\/(?:\d{4})/;
let captures2 = date.match(pattern2);
console.info(captures2);

let date2 = "02/02/2017";

// Use a backreference to a previous capture with \1
// The number 1 means that we are referencing the first (and only) capture in 
// the pattern.  This pattern checks if the month and day are the same.
let sameDayMonthPattern = /(\d{1,2})\/\1\/\d{4}/;

let fails = sameDayMonthPattern.test(date);
let succeeds = sameDayMonthPattern.test(date2);

console.info(fails); // false
console.info(succeeds); // true