"""
Experiment with implementing a virtual subclass of an ABC
Author: Andrew Jarombek
Date: 11/28/2018
"""

from Exercise import Exercise


@Exercise.register
class Elliptical:

    def __init__(self, time: tuple) -> None:
        self.__time = time

    @property
    def time(self):
        return self.__time


if __name__ == '__main__':
    Elliptical((60, 0))
