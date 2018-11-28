"""
Investigate the Sequence protocol.  To implement the Sequence protocol in Python a class needs a
__len__ and __getitem__ special method.
Author: Andrew Jarombek
Date: 11/27/2018
"""

import numbers


class TrailMap:

    trail_item_keys = ['name', 'dist']

    def __init__(self, trails):
        """
        Construct a new TrailMap object which contains a list of all the trails in a park
        :param trails: a list of trails, which should be represented as a tuple formatted
        as: (trail_name, distance)
        """
        self.__trails = trails

    @property
    def trails(self):
        return self.__trails

    def __len__(self):
        """
        Special method to get the length of the object
        :return: the length of the internal __trails list
        """
        return len(self.__trails)

    def __getitem__(self, item):
        """
        Special method to get an item at a position in the object.  Since TrailMap implements
        both __len__ and __getitem__, it follows the Sequence protocol.
        :param item: A position to search for an item
        :return: A trail in the trail map
        """
        cls = type(self)
        if isinstance(item, slice):
            return cls(self.__trails[item])
        elif isinstance(item, numbers.Integral):
            return self.__trails[item]
        else:
            error_message = '{.__name__} indices must be integers'
            raise TypeError(error_message.format(cls))


if __name__ == '__main__':
    mrp_trails = [('Main Road', 1.3), ('Swamp Trail', 2.2), ('Laurel Trail', 1.8)]
    mrp_trail_map = TrailMap(mrp_trails)

    # Since TrailMap implements the Sequence protocol, slicing now works.
    # As expected, len() and index accesses work as well
    assert len(mrp_trail_map) is 3
    assert mrp_trail_map[0] == ('Main Road', 1.3)

    last_two_trails = mrp_trail_map[1:]
    first_and_last_trails = mrp_trail_map[0::2]

    assert last_two_trails.trails == [('Swamp Trail', 2.2), ('Laurel Trail', 1.8)]
    assert first_and_last_trails.trails == [('Main Road', 1.3), ('Laurel Trail', 1.8)]

    # The following access throws a TypeError: TrailMap indices must be integers
    # mrp_trail_map['1']

    # If no __iter__() special method exists, Python falls back to __getitem__()
    for trail in mrp_trail_map:
        assert type(trail) is tuple
        print(trail)
