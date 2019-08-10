#!/usr/bin/env groovy
import groovy.transform.EqualsAndHashCode
import groovy.transform.TupleConstructor

/**
 * Testing for type equality in Groovy and comparing it to Java.
 * @author Andrew Jarombek
 * @since 8/10/2019
 */

// While Java uses == and Object.equals() to check for equality, Groovy uses ==, Object.equals(), and Object.is()
def first = "Andy"
def last = "Jarombek"
def firstAgain = "Andy"

/**
 * Class representing a ball of yarn.  Through its AST annotations, Yarn has a constructor to pass in properties
 * through and functional equals() and hashCode() methods.
 */
@TupleConstructor
@EqualsAndHashCode
class Yarn {
    String fiber
    String color
    int yards
}

def firstYarnRoll = ["Polyester", "Pitter Patter", 70] as Yarn
def secondYarnRoll = ["Polyester", "Pitter Patter", 70] as Yarn

// Groovy uses == to check for value equality, unlike Java which uses equals().  In Java,
// only primitives use == to test value equality.
assert firstYarnRoll == secondYarnRoll

// You can still use equals() and get the same result as ==.
assert firstYarnRoll.equals(secondYarnRoll)

// Since == tests value equality, Groovy needed another way to test reference equality.  The GDK adds an is() method
// to the class Object to fulfill this purpose.
assert !firstYarnRoll.is(secondYarnRoll)

// In Java, this will actually return 'true' because Java caches strings at the same reference location in memory.
// String is the only object type in Java (that I know of!) that has this behavior.
assert first == firstAgain