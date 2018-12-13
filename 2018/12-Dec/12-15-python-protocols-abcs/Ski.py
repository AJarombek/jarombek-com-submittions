"""
Experiment with extending an ABC through duck typing
Author: Andrew Jarombek
Date: 11/28/2018
"""

from Exercise import Exercise


class Ski:

    def __init__(self, time: tuple) -> None:
        self.__time = time

    @property
    def time(self) -> tuple:
        return self.__time

    def is_exercise(self) -> bool:
        """
        Determine whether the skiing was an exercise or not.  With this method, 'Ski' is considered an 'Exercise'
        through duck typing.
        :return: True if the ski is an exercise, False otherwise
        """
        minutes, seconds = self.time
        return minutes + seconds > 0


if __name__ == '__main__':
    nordic_ski = Ski((30, 0))

    # Instances and Subclasses of Exercise via __subclasshook__()
    assert isinstance(nordic_ski, Exercise)
    assert issubclass(Ski, Exercise)
