/*
 * Author: Andrew Jarombek
 * Date: 6/22/2018
 * Demonstrate bit fields in C
 */

#define VALIDATED 01
#define SUBSCRIBED 02
#define ADMIN 04

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// Type to hold a user construct
typedef struct {
    char first[31];
    char last[31];
    unsigned int statusField : 3;
} User;

void addStatus(User *user, unsigned int change);
void printUser(User user);

int main()
{
    User andy;
    strcpy(andy.first, "Andrew");
    strcpy(andy.last, "Jarombek");

    // Set the status as 000
    andy.statusField = 0;

    printUser(andy);

    // Change the status from 000 to 001
    addStatus(&andy, VALIDATED);

    printUser(andy);
}

/**
 * Add a status to the bitfield on a user type
 * @param user - a User type
 * @param change - the status to add to the bitfield
 */
void addStatus(User *user, unsigned int change) {
    // Equivalent of status = status | change.  Combines the "on" bits
    // from the existing statuses on the bitfield and the new status bits
    user->statusField |= change;
}

/**
 * Print out a User types properties
 * @param user - a User type
 */
void printUser(User user) {
    printf("%s, %s, %d \n", user.first, user.last, user.statusField);
}
