// Author: Andrew Jarombek
// Date: 11/18/2017
// Sort a list in swift with a closure

struct Person {
let first: String 
let last: String 
}

var list = [Person(first: "Andrew", last: "Jar."), Person(first: "Thomas", last: "Cau."),
   Person(first: "Joe", last: "Smi."), Person(first: "Ben", last: "Fis.")]

print(list)

list.sort {
    $0.last < $1.last
}

print(list)