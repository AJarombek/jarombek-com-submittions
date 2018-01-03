// Author: Andrew Jarombek
// Date: 1/2/2018
// The difficulties of Strings in Swift 3

import UIKit

var hello = "hello my name is andy 😺"

hello.characters.count

// The characters object is a collection of Character objects
let chars: String.CharacterView = hello.characters

// You can peform operations on the characters collection just like any other
chars.forEach {
    char -> Void in
    
    print(char)
}

// Filter out all 'a' characters
var filtered = chars.filter {$0 != "a"}

let catEmoji = filtered.popLast() // '😺'

// Convert back to a string
String(filtered)