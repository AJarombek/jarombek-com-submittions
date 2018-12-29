/**
 * C++ file for a running exercise API
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#include "Run.h"
#include <iostream>

using namespace std;

// Run class constructor which takes in the distance of the run in miles
// and the minutes:seconds that the run took to complete.
Run::Run(const double distance, const int minutes, const int seconds)
{
    // Run class invariants
    if (distance < 0) throw invalid_argument{"Distance must be > 0"};
    if (minutes < 0) throw invalid_argument{"Minutes must be > 0"};
    if (seconds < 0) throw invalid_argument{"Seconds must be > 0"};

    this->distance = distance;
    this->minutes = minutes;
    this->seconds = seconds;
}

// Calculate the mile pace of the run.  The pace is returned in seconds.
double Run::pace()
{
    return ((this->minutes * 60) + this->seconds) / this->distance;
}