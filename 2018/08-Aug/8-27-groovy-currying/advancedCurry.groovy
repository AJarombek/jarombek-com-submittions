#!/usr/bin/env groovy

/**
 * Using composition with currying
 * @author Andrew Jarombek
 * @since 8/22/2018
 */

class Pace {
    int minutes
    int seconds

    static int milePace(double miles, Pace pace) {
        def seconds = (pace.minutes * 60) + pace.seconds
        return seconds / miles
    }

    @Override
    String toString() {
        return "Pace: $minutes:${seconds >= 10 ? seconds : "0${seconds}"}"
    }
}

def toPace = {int seconds -> new Pace(minutes: Math.floor(seconds / 60), seconds: seconds % 60)}

def pace = Pace.&milePace

def pacePer5K = pace.curry(3.106)

def composition = { f, g, x -> f(g(x)) }

def minutesPerMileFor5K = composition.curry(toPace, pacePer5K)

def pr = [minutes: 15, seconds: 27] as Pace

println minutesPerMileFor5K(pr)