/**
 * Definition for a class that represents a roll of WrappingPaper.
 * @author Andrew Jarombek
 * @date 8/17/2019
 */

#include "WrappingPaper.h"

using namespace std;

/**
 * Construct a wrapping paper object that is made by a certain company and has a specific pattern.
 * @param brand The company that made the wrapping paper.
 * @param pattern The visual pattern on the wrapping paper.
 */
WrappingPaper::WrappingPaper(string* brand, string* pattern) {
    this->brand = *brand;
    this->pattern = *pattern;
}

/**
 * Overload the == operator to test value equality between two WrappingPaper objects.
 * @param other Another WrappingPaper object to test equality with.
 * @return true if the two WrappingPaper objects have equal values, false otherwise.
 */
bool WrappingPaper::operator==(WrappingPaper &other) {
    return brand == other.brand && pattern == other.pattern;
}

/**
 * Overload the != operator to test value inequality between two WrappingPaper objects.
 * @param other Another WrappingPaper object to test equality with.
 * @return true if the two WrappingPaper objects DON'T have equal values, false otherwise.
 */
bool WrappingPaper::operator!=(WrappingPaper &other) {
    return !(*this == other);
}