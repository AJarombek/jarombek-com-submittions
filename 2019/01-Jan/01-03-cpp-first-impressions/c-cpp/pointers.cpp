/**
 * Explore C++ pointers and how they compare to C
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#include <iostream>

using namespace std;

int main() {

    // Basic pointers are the same as C
    int minutes[] = {95, 85, 15, 110, 160, 105, -1};
    int* minp = &minutes[5];

    assert(*minp == 105);

    // C++ also supplies a unary suffix '&'.  It is similar to a pointer as it 'references to' a memory location.
    // '&' differs from a pointer because it doesn't need to be prefixed with '*' to get its value.
    int& minr = minutes[6];
    assert(minr == -1);

    // Use 'nullptr' to represent no object available in memory
    int* min_null = nullptr;
}