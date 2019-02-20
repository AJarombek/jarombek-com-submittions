/**
 * Explore basic C++ concepts
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#include <iostream>

// Make names from std visible without using the 'std::' qualifier
using namespace std;

// A 'constexpr' is evaluated at compile time.  The arguments to this function must be constant
// (as defined with 'const')
constexpr double pace(double miles, int minutes, int seconds)
{
    return ((minutes * 60) + seconds) / miles;
}

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

    const auto miles = 2;
    const auto minutes = 12;
    const auto seconds = 31;

    // Invoke a 'constexpr' function with 'const' arguments
    auto run_pace = pace(miles, minutes, seconds);
    assert(run_pace == 375.5);

    // Static assertions are conducted at compile time (your IDE will pick them up!)
    // This is valid because pace() is a 'constexpr' function, so it is evaluated at compile time
    static_assert(pace(miles, minutes, seconds) == 375.5);

    static_assert(sizeof(byte) == 1);
    static_assert(sizeof(short) == 2);
    static_assert(sizeof(int) == 4);
    static_assert(sizeof(long) == 8);

    static_assert(sizeof(float) == 4);
    static_assert(sizeof(double) == 8);
}