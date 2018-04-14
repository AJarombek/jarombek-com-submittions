// Author: Andrew Jarombek
// Date: 1/3/2018
// Strings in Swift 4 are a bit nicer as they are now Collections!

var hello = "hello my name is andy 😺"

// Swift 4 Strings can use all of Collections functions
print(hello.count)

hello.forEach {
	char -> Void in 
	
	print(char)
}

// In Swift 4 since Strings are collections, we can filter directly on the String
var filtered = String(hello.filter { $0 != "a" })

filtered.removeLast() // '😺'

var runDescription = "I ran 2.12 miles at 6:47 pace on January 3rd, 2018"

// We still must use String.Index, but can now skip creating a range object
let start: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 6)
let end: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 9)

let miles = runDescription[start...end] // 2.12

// Swift 4 also introduces multi-line strings!
let stats = """
	name: andy
	date: 2018-01-03
	miles: \(miles)
"""

print(stats)