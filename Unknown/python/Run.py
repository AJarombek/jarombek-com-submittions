"""
Experiment with implementing a subclass of an ABC
Author: Andrew Jarombek
Date: 11/28/2018
"""

import numbers
from datetime import date
from Exercise import Exercise


class Run(Exercise):

    def __init__(self, miles: numbers.Real, time: tuple, run_date: date, surface: str) -> None:
        """
        Initialize a new Run object, which directly subclasses the Exercise ABC
        :param miles: the number of miles run
        :param time: the time taken to complete the run.  Should be of the form (minutes, seconds)
        :param run_date: the day that the run took place
        """
        self.__miles = miles
        self.__time = time
        self.__date = run_date
        self.__surface = surface

    @property
    def miles(self) -> numbers.Real:
        return self.__miles

    @property
    def time(self) -> tuple:
        return self.__time

    @property
    def date(self) -> date:
        return self.__date

    @property
    def surface(self) -> str:
        """
        Get the surface that the run occurred on
        :return: a string describing the running surface
        """
        return self.__surface

    def __str__(self) -> str:
        """
        Create a string representation of the run
        :return: a string
        """
        return '(Ran {} miles in {}:{} on {} on {})'.format(self.miles, self.time[0], self.time[1],
                                                            self.date, self.surface)


if __name__ == '__main__':
    # Print out the method resolution order for 'Run'
    print(Run.__mro__)

    run = Run(1.14, (10, 12), date(2018, 11, 27), 'Sand')
    assert str(run) == '(Ran 1.14 miles in 10:12 on 2018-11-27 on Sand)'

    assert isinstance(run, Exercise)
    assert issubclass(Run, Exercise)
