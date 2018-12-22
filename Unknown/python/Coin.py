"""
Coins to be used in the wallet
Author: Andrew Jarombek
Date: 12/21/2018
"""

import numbers


@numbers.Real.register
class Coin:

    # Internal dictionary for the available coin types
    types = {'penny': 0.01, 'nickel': 0.05, 'dime': 0.10, 'quarter': 0.25}

    def __init__(self, typestr: str) -> None:
        """
        Construct a new Coin.  If the coins name exists in the internal 'types' dictionary, give
        the coin a corresponding value.  Otherwise the coin is worth nothing.
        :param typestr:
        """
        self.__name = typestr

        if typestr in self.types:
            self.__value = self.types[typestr]
        else:
            self.__value = 0.0

    @property
    def value(self):
        return self.__value

    def __float__(self):
        """
        Provide the floating point value of the coin
        :return: the coins value
        """
        return self.value

    def __eq__(self, other):
        """
        Determine if an object is equal to this Coin.  If the other object is an instance of Coin, simply
        check if their floating point values are equal.  Otherwise, delegate the equivalence check elsewhere.
        :param other: Another object to compare to this Coin.
        :return: True if the two Coins are equal, False otherwise.  If the other object is not an
        instance of Coin, return NotImplemented.
        """
        if isinstance(other, Coin):
            return float(self) == float(other)
        else:
            return NotImplemented

    def __repr__(self):
        """
        Provide a string representation of the Coin
        :return: The name and value of the coin as a formatted string
        """
        return '{}({})'.format(self.__name, self.value)
