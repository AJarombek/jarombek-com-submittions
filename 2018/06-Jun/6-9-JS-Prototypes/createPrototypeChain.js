/**
 * Using Object.create() to define object prototypes
 * @author Andrew Jarombek
 * @since 6/7/2018
 */

const Exercise = {
    print: function() {
        console.info(`${this.type} ${this.miles} Miles in ${this.minutes}:${this.seconds}`)
    }
};

// Create a new object with Exercise as its prototype
// Note that using this version of Object.create() means that objects are not immutable
// if you want them to have properties
let run = Object.create(Exercise);

// Add new properties to the run object.
run.type = 'Run';
run.miles = 3.46;
run.minutes = 20;
run.seconds = 33;

run.print();

console.info(Object.getPrototypeOf(run));

// Longer more verbose implementation of Object.create(prototype, object)
// Now at least the object can be implicitly immutable
let secondRun = Object.create(Exercise, {
    type: {value: 'Run'},
    miles: {value: 4.11},
    minutes: {value: 29},
    seconds: {value: 57}
});

secondRun.print();