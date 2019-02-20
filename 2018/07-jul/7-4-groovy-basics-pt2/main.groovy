#!/usr/bin/env groovy
import groovy.transform.TypeChecked

/**
 * Invoking Groovy classes
 * @author Andrew Jarombek
 * @since 7/3/2018
 */

def andy = new Person('Andrew', 'Jarombek')

// Unlike Java, == is uses for equality and not identity.
// In Java we would have to use equals() here
assert andy.toString() == 'Person(Andrew, Jarombek)'

// Objects can be initialized through a list notation
Person tom = ['Thomas', 'Cauliflower']
assert tom.toString() == 'Person(Thomas, Cauliflower)'

// When there is no explicit constructor, named arguments must be passed to the
// implicit constructor
def lily = new Cat(name: 'Lily', breed: 'Russian Blue')
assert lily.toString() == 'Cat(name:Lily, breed:Russian Blue)'

assert lily.sayHello() == 'Lily says Meow!'

// An example of a constructor that does not pass an argument for every field
def joe = new Cat(name: 'Joe')
assert joe.toString() == 'Cat(name:Joe, breed:null)'

// Accessor methods are added to public class properties by default
assert joe.getName() == 'Joe'

// This does not actually access the field called 'name',
// it is a shortcut to call the accessor method
assert joe.name == 'Joe'

joe.setBreed('Maine Coon')
assert joe.breed == 'Maine Coon'

def immutableEvan = new ImmutablePerson(first: 'Evan', last: 'Gravey')
def dotty = new ImmutableCat(name: 'dotty', breed: 'Bengal', owner: immutableEvan)
assert dotty == new ImmutableCat(name: 'dotty', breed: 'Bengal', owner: immutableEvan)

try {
    dotty.breed = 'Siamese'
    assert false
} catch (ReadOnlyPropertyException ex) {
    assert ex.message == 'Cannot set readonly property: breed for class: ImmutableCat'
}

/* Enforce Static Typing in Groovy */

// It is fine to call the factorial() function before it is declared.  This is because a groovy
// script is fully constructed before execution instead of line by line
assert factorial(-1) == -1
assert factorial(0) == 1
assert factorial(1) == 1
assert factorial(2) == 2
assert factorial(3) == 6
assert factorial(4) == 24

// Type Errors are caught at compile time instead of run time with this annotation
@TypeChecked
static int factorial(int number) {
    switch (number) {
        case { int n -> (n < 0) }:
            return -1
            break
        case 0..1:
            return 1
            break
        default:
            return computeFactorial(number)
    }
}

/**
 * Compute a factorial number
 * @param number - the number to build the factorial on (ex. number=3 : 3! = 3 * 2 * 1)
 * @return the factorial number
 */
static int computeFactorial(int number) {
    int answer = 1
    2.upto(number) { n ->
        answer *= n
    }

    return answer
}