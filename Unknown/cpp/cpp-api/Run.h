/**
 * Header file for a running exercise API
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

// Header file include guards to ensure that a header file isn't added to a C++ file multiple times.
// You can also use: #pragma once
#ifndef CPP_RUN_H
#define CPP_RUN_H

class Run {
public:
    Run(double distance, int minutes, int seconds);
    double pace();
private:
    double distance;
    int minutes;
    int seconds;
};

#endif //CPP_RUN_H