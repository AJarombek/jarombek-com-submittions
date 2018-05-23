/**
 * Testing out immutable objects in TypeScript
 * @author Andrew Jarombek
 * @since 5/21/2018
 */
var Run = /** @class */ (function () {
    function Run(name, miles, time) {
        this.date = Date.now();
        this.name = name;
        this.miles = miles;
        this.time = time;
    }
    return Run;
}());
var todaysRun = new Run("Beach Run", 3.26, { minutes: 25, seconds: 56 });
console.info(todaysRun);
// You can't change the name property because it is read only
// todaysRun.name = "Trail Run";
