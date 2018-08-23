#!/usr/bin/env groovy

/**
 * Demonstrate how to use currying with Groovy closures
 * @author Andrew Jarombek
 * @since 8/17/2018
 */

// Closure that prints out a workout
def workout = { type, miles, minutes, seconds ->

    def min = minutes >= 10 ? minutes : "0$minutes"
    def sec = seconds >= 10 ? seconds : "0$seconds"

    return "$type $miles miles in $min:$sec"
}

assert workout("Ran", 4.7, 32, 5) == "Ran 4.7 miles in 32:05"

// Bind the 'type' argument to the value 'Ran'.  All exercises that use the new curried closure will be runs
def run = workout.curry("Ran")

assert run(4.58, 29, 18) == "Ran 4.58 miles in 29:18"

// Bind the 'type' argument to the value 'Ran' and the 'miles' argument to the 5K conversion to miles
def run5K = workout.curry("Ran", 3.106)

assert run5K(15, 27) == "Ran 3.106 miles in 15:27"

// Bind the second argument in the workout closure to the 5K conversion to miles
def fiveK = workout.ncurry(1, 3.106)

assert fiveK("Biked", 15, 0) == "Biked 3.106 miles in 15:00"