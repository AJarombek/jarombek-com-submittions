#!/usr/bin/env groovy

/**
 * Using composition with currying
 * @author Andrew Jarombek
 * @since 8/22/2018
 */

/**
 * Time class that represents a running pace/time in minutes and seconds
 */
class Time {
    int minutes
    int seconds

    /**
     * Calculate the mile pace for a run given a distance in miles and a certain minutes and seconds
     * @param miles - the mileage of the run
     * @param time - the time it took to complete the run
     * @return the pace of the run
     */
    static int milePace(double miles, Time time) {
        def seconds = (time.minutes * 60) + time.seconds
        return seconds / miles
    }

    @Override
    String toString() {
        return "Time: $minutes:${seconds >= 10 ? seconds : "0${seconds}"}"
    }
}

// Closure to convert an integer representing seconds into a Time object
def toPace = {int seconds -> new Time(minutes: Math.floor(seconds / 60), seconds: seconds % 60)}

// Use the milePace() static method as a closure.  This gives us access to the curry() method
def pace = Time.&milePace

// Curry the pace closure to calculate paces for 5K races
def pacePer5K = pace.curry(3.106)

// The purpose of a composition closure is to combine two other closures
def composition = { f, g, x -> f(g(x)) }

// Create a composition of the pacePer5K closure and the toPace closure.  The resulting closure takes in a
// 5K time and returns the pace of the 5K represented by a Time object.
def minutesPerMileFor5K = composition.curry(toPace, pacePer5K)

def pr = [minutes: 15, seconds: 27] as Time

assert minutesPerMileFor5K(pr).toString() == "Time: 4:58"

def trackTT = [minutes: 16, seconds: 42] as Time

assert minutesPerMileFor5K(trackTT).toString() == "Time: 5:22"