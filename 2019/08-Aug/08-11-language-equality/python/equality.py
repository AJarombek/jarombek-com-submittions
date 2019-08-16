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
