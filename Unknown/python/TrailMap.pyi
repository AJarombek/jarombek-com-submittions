"""
A stub file that adds type hints to the TrailMap class
Author: Andrew Jarombek
Date: 11/27/2018
"""

class TrailMap:

    # TrailMap constructor
    def __init__(self, trails: list(tuple)) -> None:
        self.__trails = trails

    # Property getter method
    def trails(self) -> tuple: ...

    # Python special methods which implement the Sequence protocol
    def __len__(self) -> int: ...
    def __getitem__(self, item) -> TrailMap: ...