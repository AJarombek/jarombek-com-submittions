/**
 * Demonstrate how equality works in C++.
 * @author Andrew Jarombek
 * @date 8/17/2019
 */

#include <iostream>
#include <typeinfo>
#include <cassert>
#include "WrappingPaper.h"

using namespace std;

/**
 * Struct representing a ball of yarn.
 */
typedef struct Yarn {
    string fiber;
    string color;
    int yards;

    /**
     * Overload the == operator to test value equality between two Yarn structs.
     * @param other Another Yarn struct to test equality with.
     * @return true if the two Yarn structs have equal values, false otherwise.
     */
    bool operator == (Yarn& other) {
        return fiber == other.fiber
             && color == other.color
             && yards == other.yards;
    }

    /**
     * Overload the != operator to test value inequality between two Yarn structs.
     * @param other Another Yarn struct to test equality with.
     * @return true if the two Yarn structs DON'T have equal values, false otherwise.
     */
    bool operator != (Yarn& other) {
        return !(*this == other);
    }

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
    Yarn yarn1 = {"Polyester", "Pitter Patter", 210};
    Yarn& yarn2 = yarn1;
    Yarn yarn3 = {"Polyester", "Pitter Patter", 210};
    Yarn yarn4 = {"Polyester", "Vanilla", 70};

    // Use == and != to test the structs for value equality.
    assert(yarn1 == yarn2);
    assert(yarn2 == yarn3);
    assert(yarn3 != yarn4);

    // Use == and != on the pointers of the structs to test for reference equality.
    assert(&yarn1 == &yarn2);
    assert(&yarn2 != &yarn3);
    assert(&yarn3 != &yarn4);

    // The last assertion can be made at compile time instead of runtime.
    static_assert(
        &yarn3 != &yarn4,
        "Assertion Error: Different valued structs have the same memory location at compile time."
    );

    // Classes can also overload the == and != operators.
    string brand = "Hallmark";
    string pattern = "Disney Princess";
    string unknown = "Unknown";

    WrappingPaper wrappingPaper1 = {&brand, &pattern};
    WrappingPaper* wrappingPaper2 = &wrappingPaper1;
    WrappingPaper wrappingPaper3 = {&brand, &pattern};
    WrappingPaper wrappingPaper4 = {&unknown, &unknown};

    // Use == and != to test the objects for value equality.
    assert(wrappingPaper1 == *wrappingPaper2);
    assert(*wrappingPaper2 == wrappingPaper3);
    assert(wrappingPaper3 != wrappingPaper4);

    // Use == and != on the pointers of the objects to test for reference equality.
    assert(&wrappingPaper1 == wrappingPaper2);
    assert(wrappingPaper2 != &wrappingPaper3);
    assert(&wrappingPaper3 != &wrappingPaper4);

    // The last assertion can be made at compile time instead of runtime.
    static_assert(
        &wrappingPaper3 != &wrappingPaper4,
        "Assertion Error: Different valued objects have the same memory location at compile time."
    );
}