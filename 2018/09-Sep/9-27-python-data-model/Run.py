"""
An object that represents a run
Author: Andrew Jarombek
Date: 9/20/2018
"""


class Run:

    def __init__(self, miles=0, minutes=0, seconds=0):
        """
        Construct a Run instance
        :param miles: the miles run.  This parameter defaults to 0 if no argument is passed
        :param minutes: the minutes spent running. This parameter defaults to 0 if no argument
            is passed
        :param seconds: the seconds spent running. This parameter defaults to 0 if no argument
            is passed
        """
        miles = miles if miles >= 0 else 0
        minutes = minutes if minutes >= 0 else 0
        seconds = seconds if seconds >= 0 else 0

        self.miles = miles
        self.minutes = minutes
        self.minutes += seconds // 60
        self.seconds = seconds % 60

    def __repr__(self):
        """
        Special method used to get the string representation of an object.  It is invoked by
        both print() and str() among others.  This method is also a fallback from the __str__()
        special method
        :return: a String representation of the Run object
        """
        seconds = str(self.seconds) if self.seconds >= 10 else '0%r' % self.seconds
        return '%r miles in %r:%s' % (self.miles, self.minutes, seconds)

    def __bool__(self):
        """
        A special method used to check if the object is truthy or falsy.  An object is falsy if
        all the miles, minutes, and seconds properties equal 0.  Otherwise, it is truthy.
        :return: True if the object is truthy, False otherwise
        """
        return self.miles > 0 or self.minutes > 0 or self.seconds > 0

    def __add__(self, other):
        """
        A special method used with the addition operator (+).  This method is invoked if two
        Run objects are added together.
        :param other: another object to add to this object
        :return: a new Run object with the two previous Run objects properties added together
        """
        miles = self.miles + other.miles
        seconds = self.seconds + other.seconds
        minutes = seconds // 60
        seconds = seconds % 60
        minutes += self.minutes + other.minutes

        return Run(miles, minutes, seconds)

    def __lshift__(self, other):
        """
        A special method used with the left shift operator (<<).  This method is invoked if a
        Run object is left shifted with another object.  NOTE: This method breaks the convention
        of using the left shift operator for a bitwise operation.
        :param other: another object to left shift with this object (should be a number)
        :return: a new Run object with the other object added to the miles run of the current
            Run object.
        """
        new_mileage = self.miles + other
        new_mileage = new_mileage if new_mileage > 0 else 0

        return Run(new_mileage, self.minutes, self.seconds)
