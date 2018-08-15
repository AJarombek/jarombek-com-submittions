/**
 Demonstrate closures in Swift and how they compare to Groovy
 - Author: Andrew Jarombek
 - Date: 8/15/2018
 */

let list = [1,2,3,4,5]

var sum = 0

// In Swift, closures can modify variables in the scope they were defined in
list.forEach { item in
    sum += item
}

print(sum) // 15

/**
 A function that takes in a closure as a second argument to be used in the forEach() method of the first argument
 - parameters:
     - item: an array of integers to call forEach() on
     - closure: the closure to pass as an argument to forEach()
 */
func forEachWrapper(item: [Int], closure: (Int) -> Void) {
    item.forEach(closure)
}

forEachWrapper(item: list, closure: { sum += $0 })

print(sum)