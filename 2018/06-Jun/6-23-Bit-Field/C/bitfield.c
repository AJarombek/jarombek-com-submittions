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

void addStatus(User *user, unsigned int mask);
void removeStatus(User *user, unsigned int mask);
int containsStatus(unsigned int bitField, unsigned int mask);
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

    // Change the status from 001 to 101
    addStatus(&andy, ADMIN);

    // The user now is validated and is an admin
    printUser(andy);

    // Take away the admin rights to the user, change the status from 101 to 001
    removeStatus(&andy, ADMIN);
    printUser(andy);

    // Give the subscribed status, change the status from 001 to 011
    addStatus(&andy, SUBSCRIBED);
    printUser(andy);

    if (containsStatus(andy.statusField, VALIDATED)) {
        printf("The User IS Validated \n");
    } else {
        printf("The User IS NOT Validated \n");
    }

    if (containsStatus(andy.statusField, SUBSCRIBED)) {
        printf("The User IS Subscribed \n");
    } else {
        printf("The User IS NOT Subscribed \n");
    }

    if (containsStatus(andy.statusField, ADMIN)) {
        printf("The User IS an Admin \n");
    } else {
        printf("The User IS NOT an Admin \n");
    }
}

/**
 * Add a status to the bit field on a user type
 * @param user - a User type
 * @param mask - the status to add to the bit field
 */
void addStatus(User *user, unsigned int mask) {
    // Equivalent of statusField = statusField | mask.  Combines the "on" bits
    // from the existing statuses on the bit field and the new status bits
    user->statusField |= mask;
}

/**
 * Remove a status from the bit field on a user type
 * @param user - a User type
 * @param mask - the status to add to the bit field
 */
void removeStatus(User *user, unsigned int mask) {
    // Equivalent of statusField = statusField & ~mask.
    user->statusField &= ~mask;
}

/**
 * Check to see if a status is in the bit field
 * @param bitField - a bit field to search through
 * @param mask - a bit mask to bitwise 'and' with the bit field
 * @return 0 if the mask doesn't exist in the bit field, an integer >= 1 otherwise
 */
int containsStatus(unsigned int bitField, unsigned int mask) {
    return bitField & mask;
}

/**
 * Print out a User types properties
 * @param user - a User type
 */
void printUser(User user) {
    printf("%s, %s, %d \n", user.first, user.last, user.statusField);
}