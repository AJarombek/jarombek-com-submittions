"""
Author: Andrew Jarombek
Date: 9/19/2018
"""

from Town import Town

greenwich = Town('Greenwich', 1640, 62396, ['Riverside', 'Cos Cob', 'Old Greenwich', 'Greenwich'])

print(greenwich)
assert str(greenwich) == 'Town of Greenwich: Founded in 1640 with a Population of 62396'

print('Greenwich is %r Years Old' % len(greenwich))
assert len(greenwich) == 378

print('The Best District in Greenwich is %r' % greenwich[0])
assert greenwich[0] == 'Riverside'
assert greenwich[-2] == 'Old Greenwich'

for district in greenwich:
    print(district)
    assert district in ['Riverside', 'Cos Cob', 'Old Greenwich', 'Greenwich']
