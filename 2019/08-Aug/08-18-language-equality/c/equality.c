/**
 * Demonstrate how equality works in C.
 * Sources: [https://stackoverflow.com/a/8840096, https://stackoverflow.com/q/141720,
 * https://www.programiz.com/c-programming/library-function/string.h/strcmp]
 * @author Andrew Jarombek
 * @date 8/17/2019
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>

/**
 * Struct representing a ball of yarn.
 */
typedef struct Yarn {
    char* fiber;
    char* color;
    int yards;
} Yarn;

int yarnEqual(Yarn* y1, Yarn* y2);

int main() {
    // A small difference between C and other languages I use is that the == operator returns 0 when equality is false
    // and 1 when equality is true.
    int two = 2;
    int twoAgain = 2;

    int twosAreEqual = two == twoAgain;

    assert(two == twoAgain);
    assert(twosAreEqual == 1);

    // The == operator works with integer, floating point, and pointer types.  Therefore it works with strings, which
    // are pointers to characters or an array of characters.  When working with pointers, == checks for reference
    // equality, or in other words whether the two pointers have the same memory location.  Like other languages, C is
    // smart enough to see that two char* types hold the same strings and assigns both variables to point to the
    // same memory location.
    char* name = "Andy";
    char* nameAgain = "Andy";
    char* lastName = "Jarombek";

    assert(name == nameAgain);
    assert(name != lastName);

    // Despite the fact that name and nameAgain pointed to the same memory location, assigning a new string
    // to name doesn't impact nameAgain.
    name = "AndyJ";

    printf("%s\n", name); // AndyJ
    printf("%s\n", nameAgain); // Andy

    assert(name != nameAgain);

    // Reverting back will change the memory location again to its original position.
    name = "Andy";

    printf("%s\n", name); // Andy
    printf("%s\n", nameAgain); // Andy

    assert(name == nameAgain);

    // Therefore since an array claims a new contiguous memory location, it won't be equal to the character pointers.
    char nameArray[] = "Andy";

    assert(name != nameArray);
    assert(nameAgain != nameArray);
    assert(nameArray != lastName);

    // string.h provides a strcmp() function for testing equality between character arrays/pointers based on value.
    // Confusingly strcmp() returns 0 if the two strings are equal, another number if not.  This is different
    // than == which returns 1 if they are equal, 0 otherwise.
    int nameEqualsNameAgain = strcmp(name, nameAgain) == 0;
    int nameEqualsNameArray = strcmp(name, nameArray) == 0;
    int nameEqualsLastName = strcmp(name, lastName) == 0;

    assert(nameEqualsNameAgain);
    assert(nameEqualsNameArray);
    assert(!nameEqualsLastName);

    Yarn yarn1 = {"Polyester", "Pitter Patter", 210};
    Yarn* yarn2 = &yarn1;
    Yarn yarn3 = {"Polyester", "Pitter Patter", 210};
    Yarn yarn4 = {"Polyester", "Vanilla", 70};

    // The == and != operators can't be used on structs.
    // assert(yarn1 == yarn2);

    // To compare them for value equality, each field in the struct must be compared explicitly.
    assert(yarnEqual(&yarn1, yarn2));
    assert(yarnEqual(yarn2, &yarn3));
    assert(!yarnEqual(&yarn3, &yarn4));

    // To compare them for reference equality, convert the structs to pointers and then use ==.
    // NOTE: yarn2 is already a pointer type.
    assert(&yarn1 == yarn2);
    assert(yarn2 != &yarn3);
    assert(&yarn3 != &yarn4);

    // It's also said that while memcmp() often works when comparing structs for value equality,
    // it should not be trusted - https://stackoverflow.com/q/141720.  memcmp() returns 0 if all the bytes of memory
    // for the struct are equal, another number otherwise.
    int yarn1EqualsYarn2 = memcmp(&yarn1, yarn2, sizeof(Yarn)) == 0;
    int yarn2EqualsYarn3 = memcmp(yarn2, &yarn3, sizeof(Yarn)) == 0;
    int yarn3EqualsYarn4 = memcmp(&yarn3, &yarn4, sizeof(Yarn)) == 0;

    assert(yarn1EqualsYarn2);
    assert(yarn2EqualsYarn3);
    assert(!yarn3EqualsYarn4);

    return 0;
}

/**
 * Determine if two Yarn structs have equal values.
 * @param y1 The first Yarn struct to compare.
 * @param y2 The second Yarn struct to compare.
 * @return 1 if the two Yarn structs are equal, 0 otherwise.
 */
int yarnEqual(Yarn* y1, Yarn* y2) {
    return y1->fiber == y2->fiber
        && y1->color == y2->color
        && y1->yards == y2->yards;
}