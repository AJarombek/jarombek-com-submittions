/**
 * Traditional pre-ES6 prototype inheritance
 * @author Andrew JArombek
 * @since 6/7/2018
 */

function Exercise(type) {
    this.type = type;
    this.date = Date.now();
}

// Add properties to the Exercise prototype
// All objects created with the Exercise constructor function will have these prototype elements
Exercise.prototype.distance = 3.39;
Exercise.prototype.minutes = 25;
Exercise.prototype.seconds = 46;

const newExercise = new Exercise('Run');

console.info(newExercise);

// Able to access intems on the prototype
console.info(`${newExercise.minutes}:${newExercise.seconds}`);

// Traverse the prototype chain:
// Exercise{type, date} -> Exercise.prototype -> Object.prototype -> null
const exPrototype = Object.getPrototypeOf(newExercise);
const exProtoPrototype = Object.getPrototypeOf(exPrototype);
const exProtoProtoPrototype = Object.getPrototypeOf(exProtoPrototype);

// Exercise.prototype
console.info(exPrototype);
console.info(Exercise.prototype);

// Object.prototype
console.info(exProtoPrototype);
console.info(Object.prototype);

// null
console.info(exProtoProtoPrototype);

/* Object that has Exercise in its prototype chain */
function Run(surface) {
    Exercise.call(this, 'Run');
    this.surface = surface;
}

const firstRun = new Run('Grass');

console.info(firstRun);

// Set the prototype of Run's prototype to Exercise's prototype
Object.setPrototypeOf(Run.prototype, Exercise.prototype);

// Traverse the prototype chain of run
// run{surface} -> Run.prototype -> Exercise.prototype -> Object.prototype -> null
const runPrototype = Object.getPrototypeOf(firstRun);
const runProtoPrototype = Object.getPrototypeOf(runPrototype);

// Run.prototype
console.info(runPrototype);
console.info(Run.prototype);

// Exercise.prototype
console.info(runProtoPrototype);
console.info(Exercise.prototype);

// You can access elements from the prototype
console.info(`${firstRun.minutes}:${firstRun.seconds} on ${firstRun.surface}`);