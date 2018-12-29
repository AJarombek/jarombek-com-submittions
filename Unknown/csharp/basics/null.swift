#!/usr/bin/swift

// Explore null operators in Swift
// Created by Andy Jarombek on 12/28/2018.

import Foundation

let ageOptional: Int? = nil

// Nil coalesing operator
let age: Int = ageOptional ?? -1

assert(age == -1)

let username: String? = nil

// Nil-conditional operator
let upperUsername = username?.uppercased() ?? "Nil"

assert(upperUsername == "Nil")
