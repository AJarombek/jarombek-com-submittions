/**
 * Implement functions with the Run struct
 * @author Andrew Jarombek
 * @date 12/29/2018
 */

#include "run.h"

double pace(struct run run)
{
    return ((run.minutes * 60) + run.seconds) / run.distance;
}