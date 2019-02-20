/**
 * Header file for functions used by the Run struct
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#ifndef C_RUN_H
#define C_RUN_H

struct run {
    double distance;
    int minutes;
    int seconds;
};

double pace(struct run run);

#endif //C_RUN_H