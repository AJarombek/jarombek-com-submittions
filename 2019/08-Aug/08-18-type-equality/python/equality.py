"""
Demonstrate type equality in Python.
Author: Andrew Jarombek
Date: 8/15/2019
"""

# Value equality is tested with the == operator.
assert 1 == 1
assert "andy" == "andy"

# Python's == operator doesn't coerce types.
assert "1" != 1

# Both these strings pass value and reference equality tests.
first = "Andy"
firstAgain = "Andy"

# Both these strings pass value and reference equality tests.
forever = "rock Forever 21 but just turned thirty"
foreverAgain = str("rock Forever 21 but just turned thirty")

# These strings pass value equality tests but not reference equality tests.
desc = "Andrew Jarombek is a big fan of cats 🐱"
descAgain = "Andrew Jarombek is a big fan of cats " + "🐱"

assert first == firstAgain
assert forever == foreverAgain
assert desc == descAgain

# Reference is tested with the 'is' keyword.
# Similar to Java, Python caches strings in certain implementations.  Be careful, two strings that past the value
# equality test may not pass the reference equality test.
assert first is firstAgain
assert forever is foreverAgain
assert desc is not descAgain

# Every Python type is an object. Each object in Python is assigned a unique identifier.  You can think this unique
# identifier as the memory location of the object.
assert id(first) == id(firstAgain)
assert id(forever) == id(foreverAgain)
assert id(desc) != id(descAgain)


class Yarn:
    """
    Class representing a ball of yarn for knitting.
    """

    def __init__(self, fiber: str, color: str, yards: int):
        """
        Construct a new yarn object.  Yarn is a plain Python object with three properties, all of which should be
        assigned values upon construction.
        :param fiber: The fiber that the yarn is made of.
        :param color: The visual color of the yarn.
        :param yards: The length of the yarn in yards.
        """
        self.fiber = fiber
        self.color = color
        self.yards = yards

    def __eq__(self, other):
        """
        Implement the built in __eq__ function to test for value equality between two Yarn objects.  Return
        NotImplemented if a Yarn object is compared to a different object.  By default __neq__ negates the result of
        __eq__, so I don't need to explicitly implement it.
        :param other: An object to compare to this Yarn object.
        :return: true if the two balls of yarn are equal, false otherwise.  NotImplemented if the second object isn't
        an instance of Yarn.
        """
        if isinstance(other, Yarn):
            return self.fiber == other.fiber and self.color == other.color and self.yards == other.yards
        else:
            return NotImplemented


# Create balls of yarn to test for equality.
yarn1 = Yarn(fiber='Polyester', color='Pitter Patter', yards=210)
yarn2 = yarn1
yarn3 = Yarn(fiber='Polyester', color='Pitter Patter', yards=210)
yarn4 = Yarn(fiber='Polyester', color='Vanilla', yards=70)

# Perform the equality tests
assert yarn1 == yarn2
assert yarn1 is yarn2
assert id(yarn1) == id(yarn2)

assert yarn2 == yarn3
assert yarn2 is not yarn3
assert id(yarn2) != id(yarn3)

assert yarn3 != yarn4
assert yarn3 is not yarn4
assert id(yarn3) != id(yarn4)
