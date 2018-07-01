#!/usr/bin/env groovy

/**
 * Exploring Basic Groovy concepts
 * @author Andrew Jarombek
 * @since 7/1/2018
 */

// Groovy allows for dynamic typing.  The variable 'hello' does not always have to be a String
def hello = "Hello"

// Groovy allows you to omit parenthesis for method calls if it has at least one argument
println hello

// Dynamic typing at work - hello is now an array
hello = ["Hello", "World"]

// String templating in Groovy
println "${hello[0]} ${hello[1]}"

def list = [1, 2, 3, 4, 5]

// Iterate over an array.  Utilize Groovy closures for the method call to each(Closure)
list.each {
    println it * it
}

// A closure is actually a Groovy object and can be defined
Closure squared = { println it * it }

// Pass the closure object to each().  These parenthesis could be omitted
list.each(squared)

// Groovy closures can also take arguments.  Instead of using the default 'it' argument
// for the iterator, use 'name' instead.
["Joe", "Tom", "Ben"].each { name ->
    // Brackets are not mandatory with string templating
    println "A Great Friend: $name"
}

// Closure with no parameter.  No return statement is needed
def author = { -> "Andrew Jarombek"}

// Doesn't print the Closure object, actually prints the return value
println "Who wrote this code: $author"

// Define a key value map
def map = [
        first: "Andrew",
        last: "Jarombek",
        age: 23,
        country: "United States",
        state: "Connecticut",
        job: "Software Developer"
]

// Groovy maps are actually just a Java LinkedHashMap
LinkedHashMap otherMap = [
        first: "Joe",
        last: "Smith"
]

// Print out each entry in the map
map.each { entry ->
    println entry
}

// Accessing elements in a map is easy, and can be done in many different ways
def state = 'state'
println "${map['first']} ${map['last']} is ${map.age} years old and lives in ${map[state]}"

def newList = []

// Looping through ten times
for (i in 0..9) {
    // Can be renamed list.add(i)
    newList.add i + 1
}

// Loop through each item in the list
for (item in newList) {
    println item * 10
}

// You can also make an array easily
def array = (1..10).toArray()
println array

// <=> is called the spaceship operator.  It is the equivalent of a Java comparator function.
// This usage of the spaceship operator will return 1 since the first value is larger
println 6 <=> 5

// It returns -1 if it was the other way around
println 6 <=> 10

// And 0 if they are equal
println 1 <=> 1

// It is also null safe
println "Hello" <=> null

// Elvis operator ?: is shorthand for a full ternary operator where the true condition matches the condition
def name = "Andrew"
def result = name ?: "No Name was supplied!"
println result

// We can then use the elvis operator in a closure
def nameNotNull = { n -> n ?: "No Name was supplied!" }
println nameNotNull("Andrew")
println nameNotNull(null)