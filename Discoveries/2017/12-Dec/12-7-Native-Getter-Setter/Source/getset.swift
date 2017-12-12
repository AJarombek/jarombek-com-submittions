// Author: Andrew Jarombek
// Date: 12/11/2017
// Get and Set in Swift are for native getters and setters.
// They are also referred to as computed properties

class Person {
    var _first: String
    var _last: String
    var first: String {
        get {
            print("Accessing First Name")
            return self._first
        }
        set {
            print("Setting First Name")
            self._first = newValue
        }
    }
    var last: String {
        get {
            print("Accessing Last Name")
            return self._last
        }
        set {
            print("Setting Last Name")
            self._last = newValue
        }
    }
    var full: String {
        print("Accessing Full Name")
        return "\(self._first) \(self._last)"
    }
    
    init() {
        _first = ""
        _last = ""
    }
}

var andy = Person()
andy.first = "Andy" // Setting First Name
andy.last = "Jarombek" // Setting Last Name

andy.first // Accessing First Name
andy.last // Accessing Last Name
andy.full // Accessing Full Name
