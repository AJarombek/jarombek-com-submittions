/**
 * Explore C pointers and how they compare to C++
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#include <assert.h>

int main() {

    // Create an array of seven integers
    int minutes[] = {95, 85, 15, 110, 160, 105, -1};

    // Declare a "pointer to" an integer
    int *minp;

    // Assign an integer pointer to an "address of" an array element
    minp = &minutes[5];

    // Get the integer that minp "points to"
    assert(*minp == 105);

    return 0;
}