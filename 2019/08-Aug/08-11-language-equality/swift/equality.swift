#!/usr/bin/swift

/**
 Demonstrate how equality works in Swift
 - Author: Andrew Jarombek
 - Date: 8/12/2019
 */

import Foundation

// Swift's basic value types can be compared for value equality with the == operator.
let name: String = "Andy"
let nameAgain: String = "Andy"

assert(name == nameAgain)

// The value types in Swift are String, Int, Float, Double, Array, and Dictionary.
let animalsComfortingMe: [String:String] = ["Dotty":"Horse", "Lily":"Bear"]
let animalsComfortingMeAgain: [String:String] = ["Dotty":"Horse", "Lily":"Bear"]

assert(animalsComfortingMe == animalsComfortingMeAgain)

// Value equality also works with the Objective-C String equivalent in Swift
let lastName: NSString = "Jarombek"
let lastNameAgain: NSString = "Jarombek"

assert(lastName == lastNameAgain)

// You can't perform reference equality on value types in Swift.  Reference equality is tested
// with the === operator.  The following examples will not compile with this note attached:
// expected an argument list of type '(AnyObject?, AnyObject?)'

// var valueTypesReferencesEqual: Bool = name === nameAgain
// var valueTypesReferencesEqual: Bool = animalsComfortingMe === animalsComfortingMeAgain

struct Yarn: Equatable {
    let fiber: String
    let color: String
    let yards: Int

    /**
     Function from the equatable protocol to overload the == operator when comparing yarn objects.
     - parameters:
     - lhs: the first ball of yarn to test for equality.
     - rhs: the second ball of yarn to test for equality.
     - returns: true if the properties in both the yarn structs are equal, false otherwise.
     */
    static func ==(lhs: Yarn, rhs: Yarn) -> Bool {
        return (lhs.fiber == rhs.fiber) && (lhs.color == rhs.color) && (lhs.yards == rhs.yards)
    }
}

let firstYarnBall = Yarn(fiber: "Polyester", color: "Pitter Patter", yards: 210)
let secondYarnBall = firstYarnBall
let thirdYarnBall = Yarn(fiber: "Polyester", color: "Pitter Patter", yards: 210)
let fourthYarnBall = Yarn(fiber: "Polyester", color: "Vanilla", yards: 70)

// Since Yarn is a struct and not a class, reference equality doesn't work.  Remember that the
// arguments to ==(lhs, rhs) must be of type AnyObject?
// assert(firstYarnBall === secondYarnBall)

assert(firstYarnBall == secondYarnBall)
assert(secondYarnBall == thirdYarnBall)
assert(thirdYarnBall != fourthYarnBall)
