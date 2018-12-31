/**
 * main C++ file for testing the running exercise API
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#include "Run.h"
#include <iostream>

using namespace std;

int main()
{
    Run run {2.0, 12, 31};
    double pace = run.pace();
    cout << pace << endl;

    auto minute_pace = (int) pace / 60;
    auto second_pace = (int) pace % 60;

    assert(minute_pace == 6);
    assert(second_pace == 15);

    return 0;
}