"""
Main script to experiment with operator overloading
Author: Andrew Jarombek
Date: 12/21/2018
"""

from Coin import Coin
from Wallet import Wallet

wallet = Wallet([Coin('quarter'), Coin('nickel'), Coin('other')])

assert str(wallet) == '[quarter(0.25), nickel(0.05), other(0.0)]'
assert float(wallet) == 0.3

wallet_2 = Wallet([Coin('quarter'), Coin('quarter')])

assert str(wallet_2) == '[quarter(0.25), quarter(0.25)]'
assert float(wallet_2) == 0.5

assert wallet != wallet_2

# Add two Wallet objects together
wallet_3 = wallet + wallet_2

assert str(wallet_3) == \
       '[quarter(0.25), nickel(0.05), other(0.0), quarter(0.25), quarter(0.25)]'
assert float(wallet_3) == 0.8

# Unable to add a Wallet with a Coin object

# TypeError: unsupported operand type(s) for +: 'Wallet' and 'Coin'
# wallet_4 = wallet_2 + Coin('quarter')
