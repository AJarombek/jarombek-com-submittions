/**
 * Testing out immutable objects in TypeScript
 * @author Andrew Jarombek
 * @since 5/21/2018
 */

/*
Define a type representing a time with minutes and seconds
https://stackoverflow.com/questions/31364693/what-is-the-type-reserved-word-in-typescript
*/
type Time = {
    readonly minutes: number;
    readonly seconds: number;
}

class Run {

    // The readonly keyword makes a property immutable
    // https://basarat.gitbooks.io/typescript/docs/types/readonly.html
    readonly name: string;
    readonly date = Date.now();
    readonly miles: number;
    readonly time: Time;

    constructor(name: string, miles: number, time: Time) {
        this.name = name;
        this.miles = miles;
        this.time = time;
    }
}

const todaysRun = new Run(
        "Beach Run",
        3.26,
        {minutes: 25, seconds: 56}
    );

console.info(todaysRun);

// You can't change the name property because it is read only
// todaysRun.name = "Trail Run";