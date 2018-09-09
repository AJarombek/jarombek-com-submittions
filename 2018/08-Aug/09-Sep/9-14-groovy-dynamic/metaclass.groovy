#!/usr/bin/env groovy

/**
 * Experiment with the Groovy MetaClass.
 * @author Andrew Jarombek
 * @since 9/9/2018
 * @source All page #s are from the book Groovy: In Action (Second Edition)
 */

MetaClass mc = List.metaClass

// The number of methods in the List object
assert mc.methods.size() == 9

// The number of methods that Groovy dynamically adds to the List object (pg. 210)
assert mc.metaMethods.size() == 288

// The number of properties in the List object
assert mc.properties.size() == 1

// Invoke methods dynamically with the meta class
assert mc.invokeMethod([1,2,3], "toListString", new Object[0]) == "[1, 2, 3]"
assert mc.invokeMethod([1,2,3], "get", [2]) == 3

// Use case for the meta class - alter the behavior of an existing Groovy objects meta class (pg. 227)
Number.metaClass {
    getMiles = { delegate }
    getKilometers = { delegate / 1.60934.miles }
    getMeters = { delegate / 1000.kilometers }

    getToMiles = { delegate }
    getToKilometers = { delegate * 1.60934.toMiles }
    getToMeters = { delegate * 1000.toKilometers }
}

// Distance I ran yesterday
def run = 10.6.miles
assert run.toKilometers.round(2) == 17.06
assert run.toMeters.round(2) == 17059.00

// My favorite track event
def run5k = 5.kilometers
assert run5k.toMiles.round(2) == 3.11
assert run5k.toMeters.round(2) == 5000