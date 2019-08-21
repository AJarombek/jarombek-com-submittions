/**
 * Header file for a class that represents a roll of WrappingPaper.
 * @author Andrew Jarombek
 * @date 8/17/2019
 */

#ifndef CPP_WRAPPINGPAPER_H
#define CPP_WRAPPINGPAPER_H

#include <string>

using namespace std;

class WrappingPaper {
public:
    WrappingPaper(string* brand, string* pattern);
    bool operator == (WrappingPaper& other);
    bool operator != (WrappingPaper& other);
private:
    string brand;
    string pattern;
};

#endif // CPP_WRAPPINGPAPER_H