#!/usr/bin/env groovy

/**
 * Demonstrating Groovy's Dynamic Type System
 * @author Andrew Jarombek
 * @since 7/13/2018
 */

package types

import groovy.transform.CompileStatic

// Groovy is dynamically typed - meaning that types are checked at runtime instead of compile time
// This leads to interesting bugs since at runtime groovy uses Java's type system
// The following code will compile yet fail at runtime since casting an ArrayList to an Integer is not valid
// Integer hello = ['h', 'e', 'l', 'l', 'o']
// println hello

// Interestingly the same code will pass if you use a String type, casting an ArrayList to a String
String helloString = ['h', 'e', 'l', 'l', 'o']
println helloString

// Groovy is also optionally typed - meaning the type definitions can be omitted
// When a type is not specified, the type is Object.  It does not mean there is no type
def hiWorld = "Hello World"

assert hiWorld instanceof Object
assert hiWorld instanceof String

// Groovy's dynamic nature along with optional typing allows for duck typing
def static outputAll(item) {
    def str = "["
    item.each {
        str += "${it}"
    }
    str += "]"
    return str
}

def outputList = outputAll([1,2,3,4,5])
def outputMap = outputAll([name:'Andy', age:'23'])

println outputMap
println outputList

// The compile static annotation causes a method or class to be statically compiled to bytecode
// This annotation can greatly improve performance, taking advantage of JVM optimizations
@CompileStatic
def execIntClosure(int param, Closure closure) {
    closure(param)
}

assert execIntClosure(5) { i -> i * 10 } == 50