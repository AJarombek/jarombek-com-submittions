/**
 * Test out the Run struct and corresponding functions
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#include "run.h"
#include <stdio.h>
#include <assert.h>

int main()
{
    struct run run1;
    run1.distance = 2;
    run1.minutes = 12;
    run1.seconds = 31;

    double run_pace = pace(run1);

    printf("%f", run_pace);
    assert(run_pace == 375.5);
}