/**
 * Explore multiple inheritance in C++
 * @author Andrew Jarombek
 * @date 12/17/2018
 */

#include <iostream>

using namespace std;

// Defines a generic tree
class Tree {
protected:
    int feet;
    int inches;
public:
    Tree(int feet, int inches) {
        cout << "Tree()" << endl;
        this->feet = feet;
        this->inches = inches;
    }

    // Calculates the height of the tree in inches
    int height() {
        return (this->feet * 12) + this->inches;
    }
};

// Defines a Christmas tree
class ChristmasTree: virtual public Tree {
public:
    ChristmasTree(int feet, int inches): Tree(feet, inches) {
        cout << "ChristmasTree()" << endl;
    }

    string type() {
        return "Christmas";
    }
};

// Defines an evergreen tree
class EvergreenTree: virtual public Tree {
public:
    EvergreenTree(int feet, int inches): Tree(feet, inches) {
        cout << "EvergreenTree()" << endl;
    }

    string type() {
        return "Evergreen";
    }
};

// Defines a balsam fir, which is a christmas tree and an evergreen tree
class BalsamFir: public ChristmasTree, public EvergreenTree {
public:
    BalsamFir(int feet, int inches): ChristmasTree(feet, inches), EvergreenTree(feet, inches), Tree(feet, inches) {
        cout << "BalsamFir()" << endl;
    }

    bool leafPeristence() {
        return EvergreenTree::type() == "Evergreen";
    }
};

int main() {
    BalsamFir balsamFir(7, 2);

    assert(balsamFir.height() == 86);
    assert(balsamFir.leafPeristence());

    // Calling type() is ambiguous since both ChristmasTree and EvergreenTree have
    // assert(balsamFir.type());

    return 0;
}