/**
 * Demonstrate how equality works in C++.
 * @author Andrew Jarombek
 * @date 8/17/2019
 */

#include <iostream>
#include <typeinfo>
#include <cassert>

using namespace std;

/**
 * Struct representing a ball of yarn.
 */
typedef struct Yarn {
    string fiber;
    string color;
    int yards;
} Yarn;

int main() {
    // Basic type equality is mostly the same in C++ compared to C.  However, the == operator returns a boolean value
    // instead of an integer.
    int two = 2;
    int twoAgain = 2;

    // 'auto' can be replaced with 'bool'
    auto twosAreEqual = two == twoAgain;

    assert(two == twoAgain);

    cout << typeid(twosAreEqual).name() << endl; // 'b' for boolean
    assert(twosAreEqual);

    // In C++, comparing a string to a character array correctly determines value equivalence.
    // This wasn't the case in C.  This is because the == operator is overloaded for strings in C++.
    string name = "Andy";
    string nameAgain = "Andy";
    string lastName = "Jarombek";
    char nameArray[] = "Andy";

    assert(name == nameAgain);
    assert(name == nameArray);
    assert(name != lastName);

    // You can also use compare() to test for string equality, although it isn't recommended.
    assert(name.compare(nameAgain) == 0);

    // Similar to C, there is no == operator for structs by default.  Unlike C, in C++ the == operator can be
    // overloaded to work with two structs of the same type.
}