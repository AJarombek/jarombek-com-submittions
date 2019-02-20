"""
Experiment with multiple inheritance using Christmas trees
Author: Andrew Jarombek
Date: 12/12/2018
"""


class Tree:
    """ Class representing a generic tree """

    def __init__(self, height: tuple) -> None:
        self.__height = height

    @property
    def height(self) -> tuple:
        """
        The height of the tree in feet and inches
        :return: A tuple of form ('feet', 'inches')
        """
        return self.__height


class ChristmasTree(Tree):
    """ Class representing a tree used for Christmas """

    def __init__(self, height: tuple) -> None:
        super().__init__(height)
        self.__type = 'Christmas'

    def type(self) -> str:
        """
        The type of tree
        :return: 'Christmas'
        """
        return self.__type


class EvergreenTree(Tree):
    """ Class representing a tree that keeps its foliage year round """

    def __init__(self, height: tuple) -> None:
        super().__init__(height)
        self.__type = 'Evergreen'

    def type(self) -> str:
        """
        The type of tree
        :return: 'Evergreen'
        """
        return self.__type


class BalsamFir(ChristmasTree, EvergreenTree):
    """ Class representing a balsam fir Christmas tree """

    def __init__(self, height: tuple) -> None:
        super().__init__(height)

    def leaf_persistence(self) -> str:
        """
        Determine if the leaves persist or not
        :return: either 'Evergreen' or 'Deciduous'
        """
        return EvergreenTree.type(self)


if __name__ == '__main__':
    balsam = BalsamFir((7, 2))

    assert balsam.height == (7, 2)
    assert balsam.type() == 'Christmas'
    assert balsam.leaf_persistence() == 'Evergreen'

    # The method resolution order shows that the class 'ChristmasTree' comes before 'EvergreenTree'
    assert BalsamFir.__mro__ == (BalsamFir, ChristmasTree, EvergreenTree, Tree, object)
