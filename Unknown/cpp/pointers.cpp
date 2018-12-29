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
    int *minp = &minutes[5];

    assert(*minp == 105);
}