"""
Experiment with operator overloading using a Wallet that contains different coins
Author: Andrew Jarombek
Date: 12/21/2018
"""

import numbers


class Wallet:

    def __init__(self, contents: list = None) -> None:
        """
        Construct a new wallet from a list of contents.  Contents only have value if they are
        an instance of the numbers.Real ABC.
        :param contents: A list of items in the wallet
        """
        if contents is None:
            self.contents = list()
        else:
            self.contents = [item if isinstance(item, numbers.Real) else 0 for item in contents]

    def __len__(self):
        """
        Determine the length of the wallet
        :return: the number of coins in the wallet
        """
        return len(self.contents)

    def __add__(self, other):
        """
        Add two objects together if they have an internal 'contents' property.  One object I
        know is an instance of Wallet, but the other might not be.
        :param other: Another object to add into a wallet
        :return: a new Wallet object with both objects contents combined
        """
        try:
            combined_contents = self.contents + other.contents
            return Wallet(combined_contents)
        except AttributeError:
            return NotImplemented

    def __radd__(self, other):
        """
        Reverse of the __add__ special method.  Simply delegate to the __add__ special method
        :param other: Another object to add into a wallet
        :return: a new Wallet object with both objects contents combined
        """
        return self + other

    def __eq__(self, other):
        """
        Determine if an object is equal to this Wallet.  Only perform an equality operation
        here if the other object is an instance of Wallet.
        :param other: Another object to compare to this object
        :return: True if both Wallets are equal, False otherwise.  If the other object is not an
        instance of Wallet, return NotImplemented.
        """
        if isinstance(other, Wallet):
            return (len(other) == len(self) and
                    all(x == y for x, y in zip(other.contents, self.contents)))
        else:
            return NotImplemented

    def __repr__(self):
        """
        Provide a string representation of the Wallet
        :return: The contents of the Wallet as a string
        """
        return str(self.contents)

    def __float__(self):
        """
        Convert the Wallet into a floating point number.  This method adds up the value of all the
        coins in the Wallet
        :return: the value of the Wallet
        """
        return sum([float(item) for item in self.contents])
