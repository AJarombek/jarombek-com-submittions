#!/usr/bin/swift

/*
 Experiment with multiple inheritance (with protocols) in Swift
 - Author: Andrew Jarombek
 - Date: 12/16/2018
 */

import Foundation

/**
 Create a protocol for a generic tree.  Protocols are similar to interfaces from Java
 */
protocol Tree {

    /**
     Get the height of the tree
     */
    func height() -> [String:Int]
}

typealias ChristmasTree = Tree

/**
 Add an extension to the Tree protocol for Christmas trees
 */
extension ChristmasTree {

    var type: String {
        return "Christmas"
    }
}

typealias EvergreenTree = Tree

/**
 Add an extension to the Tree protocol for Evergreen trees
 */
extension EvergreenTree {
    // Can't redeclare var 'type', since it already exists in ChristmasTree

    var leaf_persistence: String {
        return "Evergreen"
    }
}

/**
 Implement the Tree protocol with the ChristmasTree and EvergreenTree extensions
 */
class BalsamFir: Tree {

    let feet: Int
    let inches: Int

    init(height: [String:Int]) {
        self.feet = height["feet"] ?? 0
        self.inches = height["inches"] ?? 0
    }

    /**
     Implement the height() method defined in the Tree protocol.
     */
    func height() -> [String:Int] {
        return ["feet": self.feet, "inches": self.inches]
    }

    /**
     Determine whether or not the leaves on the tree persist through winter
     */
    func leafPersistence() -> Bool {
        return self.leaf_persistence == "Evergreen"
    }
}

// Test scripts

let balsam = BalsamFir(height: ["feet": 7, "inches": 2])

assert(balsam.type == "Christmas")
assert(balsam.height() == ["feet": 7, "inches": 2])
assert(balsam.leafPersistence())