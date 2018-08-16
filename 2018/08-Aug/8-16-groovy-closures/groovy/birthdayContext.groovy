#!/usr/bin/env groovy

/**
 * Explore the birthday context (confusingly referred to as a closure in other languages) of a
 * Groovy closure.
 * @author Andrew Jarombek
 * @since 8/15/2018
 */

class BirthdayContext {
    def age = 0
    def closure = { println age }
}

def age = 23
def birthObject = new BirthdayContext()

assert birthObject.closure.owner == birthObject
assert birthObject.closure.thisObject == birthObject
assert birthObject.closure.delegate == birthObject

// Prints 0 for age instead of 23
birthObject.closure()

class OtherContext {
    def age = 21
}

def otherObject = new OtherContext()

// You can manually set the delegate of a closure - a user defined object that a closure will use
birthObject.closure.delegate = otherObject

assert birthObject.closure.owner == birthObject
assert birthObject.closure.thisObject == birthObject
assert birthObject.closure.delegate == otherObject

// Will print 0 for age instead of 21
birthObject.closure()

// Change the delegation strategy to delegate first, meaning variables will be checked for on the delegate object
// first before the owner object.  The default strategy is OWNER_FIRST
// https://bit.ly/2BaQoye
birthObject.closure.resolveStrategy = Closure.DELEGATE_FIRST

// Will now print 21 instead of 0
birthObject.closure()

// with() takes a closure and sets the delegation strategy to DELEGATE_FIRST.  The object with() is called upon
// is used as the delegate
otherObject.with {
    delegate.age = 18
}

// Will now print 18
birthObject.closure()