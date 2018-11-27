"""
Investigate __slots__ for classes
Author: Andrew Jarombek
Date: 11/27/2018
"""

from datetime import date


class CodeWritten:

    # The __slots__ tuple replaces the per-instance __dict__
    __slots__ = ('__day', '__language', '__amount')

    def __init__(self, day: date, language: str, amount: int) -> None:
        """
        Construct a new CodeWritten instance which represents the number of lines coded
        in a specific language on a certain day
        :param day: The day the code was written
        :param language: The language the code was written in
        :param amount: The number of lines coded
        """
        self.__day = day
        self.__language = language
        self.__amount = amount

    @property
    def day(self) -> date:
        return self.__day

    @property
    def language(self) -> str:
        return self.__language

    @property
    def amount(self) -> int:
        return self.__amount


if __name__ == '__main__':
    python_today = CodeWritten(date.today(), 'Python', 146)
    groovy_today = CodeWritten(date.today(), 'Groovy', 50)
    yaml_today = CodeWritten(date.today(), 'YAML', 12)

    assert python_today.amount == 146
    assert python_today.language == 'Python'
    assert python_today.day == date.today()
