"""
A stub file that adds type hints to the Run class
Author: Andrew Jarombek
Date: 9/24/2018
"""

from typing import TypeAlias

RunType: TypeAlias(Run)

class Run:
    def __init__(self, miles: float = 0, minutes: int = 0, seconds: int = 0) -> RunType:
        self.miles = miles
        self.minutes = minutes
        self.seconds = seconds

    def __repr__(self) -> str: ...
    def __bool__(self) -> bool: ...
    def __add__(self, other) -> RunType: ...
    def __lshift__(self, other) -> RunType: ...