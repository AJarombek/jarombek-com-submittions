/**
 * Explore basic C++ concepts
 * @author Andrew Jarombek
 * @date 12/17/2018
 */

#include <iostream>

// Make names from std visible without using the 'std::' qualifier
using namespace std;

int main() {

    // C++ initialization notations
    int age = 23;
    double programming_experience {2.5};

    // 'auto' is used to not explicitly state the type of a variable (like 'var' in C# and Java)
    auto percentage_programming = programming_experience / age;

    assert(programming_experience > 0.10 && percentage_programming < 0.11);

    // 'const' defines a variable whose value cant change
    const string name = "Andrew Jarombek";

    cout << name << endl;
    cout << percentage_programming << endl;
}