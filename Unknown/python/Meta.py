"""
Investigate class and static methods
Author: Andrew Jarombek
Date: 11/27/2018
"""


class Meta:

    def __init__(self, first_name: str, last_name: str):
        """
        Construct a new Meta object which contains the first and last name of its creator
        :param first_name: the first name of the objects creator
        :param last_name: the last name of the objects creator
        """
        self.__first_name = first_name
        self.__last_name = last_name

    @property
    def first_name(self) -> str:
        """
        @property marks this function as a getter method.  This method gets the internal
        first_name property of Meta.
        :return: A string representing the owners first name
        """
        return self.__first_name

    @property
    def last_name(self) -> str:
        """
        This method gets the internal last_name property of Meta.
        :return: A string representing the owners last name
        """
        return self.__last_name

    def __format__(self, format_spec: str=''):
        """
        Special method to format a string representation of the Meta object
        :param format_spec: the formatting specifier
        :return: a formatted string representing the instance
        """
        components = (format(self.__dict__[prop], format_spec) for prop in self.__dict__)
        return '({}, {})'.format(*components)

    @classmethod
    def about(cls) -> None:
        """
        @classmethod alters this method to belong to the class instead of an object instance.
        Therefore the first argument to this method is the class itself instead of the object
        instance.  This method prints out some important properties of the class.
        :return: None
        """
        print('Class Name: {}'.format(cls.__name__))
        print('Class Dict: {}'.format(cls.__dict__))

    @classmethod
    def create(cls):
        """
        Alternative constructor using a class method.  In Java, this construct is known as a
        'static factory method'
        :return: An instance of Meta
        """
        return cls("Andrew", "Jarombek")

    @staticmethod
    def static_create():
        """
        Alternative constructor using a static method.  Static methods receive no special
        first method unlike class methods and plain methods.
        :return: An instance of Meta
        """
        return Meta.create()


if __name__ == "__main__":
    Meta.about()

    # Construct using the class method
    meta = Meta.create()

    # Class methods can also be called on instances
    meta.about()

    # attributes beginning with double underscores (__) are name mangled in the instance dict
    assert meta.__dict__ == {'_Meta__first_name': 'Andrew', '_Meta__last_name': 'Jarombek'}
    assert meta.__dict__['_Meta__first_name'] == 'Andrew'
    assert meta.__dict__['_Meta__last_name'] == 'Jarombek'

    assert meta.first_name == "Andrew"
    assert meta.last_name == "Jarombek"

    assert format(meta, '') == '(Andrew, Jarombek)'

    # Construct by invoking the constructor directly
    custom_meta = Meta("Andy", "J")
    assert custom_meta.first_name == "Andy"
    assert custom_meta.last_name == "J"

    assert format(custom_meta, '') == '(Andy, J)'
