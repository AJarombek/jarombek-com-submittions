"""
Class representing a locker of running shoes
Author: Andrew Jarombek
Date: 11/28/2018
"""

from collections import abc


class Locker(abc.Container):

    def __init__(self, shoes: list) -> None:
        """
        Initialize the shoe locker
        :param shoes: a list of running shoes (should be represented as a tuple)
        """
        self.shoes = shoes

    # __contains__ is mandatory because Locker subclasses abc.Container
    def __contains__(self, x: object) -> bool:
        """
        Follows the abc.Container Abstract Base Class.  Checks for an items existence in the locker.
        :param x: the item to look for in the instance
        :return: True if the item exists, False otherwise
        """
        return x in self.shoes


if __name__ == '__main__':
    # Running shoes in tuple form - (name, miles)
    running_shoes = [('Asics GT2000', 700), ('Nike Victory', 80)]
    shoe_locker = Locker(running_shoes)

    # The 'in' operator goes through __contains__()
    assert ('Nike Victory', 80) in shoe_locker
