"""
Experiment with creating a custom ABC for exercises
Author: Andrew Jarombek
Date: 11/28/2018
"""

import abc
import numbers
from datetime import date


class Exercise(abc.ABC):

    @property
    @abc.abstractmethod
    def miles(self) -> numbers.Real:
        """
        Get the distance of the exercise in miles
        """

    @property
    @abc.abstractmethod
    def time(self) -> tuple:
        """
        Get the length of the exercise in minutes and seconds
        """

    @property
    @abc.abstractmethod
    def date(self) -> date:
        """
        Get the date that the exercise occurred
        """

    # ABCs can contain concrete methods
    def pace(self) -> tuple:
        """
        Calculate the pace of the exercise in minutes and seconds
        """
        minutes, seconds = self.time
        total_seconds = (minutes * 60) + seconds
        seconds_per_mile = total_seconds / self.miles
        return seconds_per_mile / 60, seconds_per_mile % 60

    @classmethod
    def __subclasshook__(cls, c):
        """
        Help determine if a class is a subclass.  Use duck typing to make any class with an
        'is_exercise' method a subclass of Exercise
        :param c: The class to check if it is a subclass
        :return: True if the class is a subclass of Exercise, otherwise NotImplemented
        """
        print(cls)
        print(c)
        if cls is Exercise:
            if any("is_exercise" in b.__dict__ for b in c.__mro__):
                return True
        return NotImplemented


if __name__ == '__main__':
    # Print out the method resolution order for 'Exercise'
    assert Exercise.__mro__ == (Exercise, abc.ABC, object)

    assert issubclass(Exercise, abc.ABC)
