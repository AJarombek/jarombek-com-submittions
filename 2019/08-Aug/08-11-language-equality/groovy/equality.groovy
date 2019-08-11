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

// Other string equality checks
assert first.equals(firstAgain)
assert first.is(firstAgain)

assert first != last
assert !first.equals(last)
assert !first.is(last)

// == determines that two objects (x and y) are equal if x.compareTo(y) returns 0.  One of the rules about comparators
// is that x.compareTo(y) should always return 0 if x.equals(y) returns true.  However, a malicious programmer can
// override this behavior just like they can override equals() in Java.

/**
 * Class representing gift wrapping paper.
 */
@TupleConstructor
@EqualsAndHashCode
class WrappingPaper implements Comparable<WrappingPaper> {
    String brand
    String pattern

    @Override
    int compareTo(WrappingPaper o) {
        // Usually a compareTo() method would first check if the objects are equal.
        // A malicious or unknowing user might leave this check out.
        // if (Objects.equals(this, o)) return 0

        // In Groovy, the spaceship operator performs a compareTo() operation.
        // Therefore, the spaceship operator in the below context replaces the following line:
        // this.pattern.compareTo(o.pattern)
        return this.pattern <=> o.pattern
    }
}

def wrappingPaper1 = new WrappingPaper("Hallmark", "Disney Princess")
def wrappingPaper2 = new WrappingPaper("Unknown", "Disney Princess")

// is() performs reference equality as expected:
assert !wrappingPaper1.is(wrappingPaper2)

// Calling equals() directly still performs value equality as expected:
assert !wrappingPaper1.equals(wrappingPaper2)

// However, == doesn't perform value equality properly because it calls compareTo() and not equals().  The brands are
// different but == thinks the two object values are the same
assert wrappingPaper1 == wrappingPaper2

// == coerces different numeric types before comparing
assert 2.0f == 2.0
assert 2.0 == 2
assert 2l == 2
assert 1G == 1

// ... but it doesn't coerce other types before comparing
assert "2.0" != 2.0