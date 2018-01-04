// Author: Andrew Jarombek
// Date: 1/2/2018
// The difficulties of Strings in Swift 3

import UIKit

var hello = "hello my name is andy 😺"

hello.characters.count

// The characters object is a collection of Character objects
let chars: String.CharacterView = hello.characters

// You can peform operations on the characters collection just like any other collection
chars.forEach {
    char -> Void in
    
    print(char)
}

// Filter out all 'a' characters
var filtered = chars.filter {$0 != "a"}

// Remove and return the last character in collection
let catEmoji = filtered.popLast() // '😺'

// Convert back to a string
String(filtered)

String(filtered[6...7]) // returns 'my'

// The complexity of getting a substring
var runDescription = "I ran 2.12 miles at 6:47 pace on January 3rd, 2018"

// You can't use Int to specify a substring, you must use String.Index
let start: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 6)
let end: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 10)

// You also must create a range object before using it in the substring() function
let range: Range<String.Index> = start..<end

runDescription.substring(with: range) // 2.12