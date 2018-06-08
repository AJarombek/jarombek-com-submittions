/**
 * Demonstrate ES6 class syntax and how it is syntactic sugar over prototypes
 * @author Andrew Jarombek
 * @since 6/7/2018
 */

class Exercise {
    constructor(type, miles, minutes, seconds) {
        this.type = type;
        this.miles = miles;
        this.minutes = minutes;
        this.seconds = seconds;
    }

    print() {
        console.info(`${this.type} ${this.miles} Miles in ${this.minutes}:${this.seconds}`);
    }
}

class Run extends Exercise {
    constructor(miles, minutes, seconds, surface) {
        super('Run', miles, minutes, seconds);
        this.surface = surface;
    }
}

const run = new Run(1, 5, 31, 'Track');

run.print();