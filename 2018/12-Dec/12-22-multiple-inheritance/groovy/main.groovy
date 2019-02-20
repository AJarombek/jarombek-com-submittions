/**
 * Main script to test out the multiple inherited traits
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

def heightMap = [feet: 7, inches: 2]
def balsam = new BalsamFir(heightMap)

assert balsam.leafPersistence()
assert balsam.height() == '(7, 2)'

assert balsam.isChristmasTree()