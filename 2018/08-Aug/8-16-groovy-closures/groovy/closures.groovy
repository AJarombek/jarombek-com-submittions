#!/usr/bin/env groovy

/**
 * Examples of some of the basic abilities of closures in Groovy
 * @author Andrew Jarombek
 * @since 8/15/2018
 */

int number = 1

// Closures in groovy can manipulate values of variables defined in their outer scope
def timesTen = { number *= 10 }

println "Number before closure manipulation: $number"
timesTen()
println "Number after closure manipulation: $number"

static def timesTwo(val) {
    return val * 2
}

def list = [0, 1, 2, 3, 4, 5]

// These both do the same thing - you can pass a method as a closure with the '.&' syntax
list = list.collect(this.&timesTwo)
list = list.collect { it * 2 }

println list