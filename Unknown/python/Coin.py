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

    def __init__(self, typestr: str):
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
        return float(self) == float(other)

    def __repr__(self):
        return '{}({})'.format(self.__name, self.value)
