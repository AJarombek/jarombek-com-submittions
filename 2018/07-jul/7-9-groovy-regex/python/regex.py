#!/usr/bin/env python

import re

"""
Working with Regular Expressions
Author: Andrew Jarombek
Date: 7/8/2018
"""

datePattern = re.compile('\d{1,2}/\d{1,2}/\d{4}')

today = '7/8/2018'
tomorrow = 'Tomorrow is 7/9/2018.'
endOfYear = '12/31/2018'
myBirthday = 'Feb. 26, 2018'

# Test for exact matches
assert datePattern.match(today)
assert not datePattern.match(tomorrow)
assert datePattern.match(endOfYear)
assert not datePattern.match(myBirthday)

# Test for existence of a match in the string
assert datePattern.search(today)
assert datePattern.search(tomorrow)
assert datePattern.search(endOfYear)
assert not datePattern.search(myBirthday)

# Looping through RegEx matches

catPattern = re.compile('[Cc][Aa][Tt][Ss]?')

catStatements = '''
    I really like cats.  Cats, cats, CATS!
    I wish I had a cat, I would name it Cat.
'''

catAppearances = catPattern.findall(catStatements)

assert len(catAppearances) is 6
assert catAppearances[0] == 'cats'
assert catAppearances[3] == 'CATS'
assert catAppearances[5] == 'Cat'

# Looping through Regex Grouping Captures

topLanguages = '''
Top 5 Favorite Programming Languages (as of 7/8/2018)
    1. Java 
    2. JavaScript 
    3. Python 
    4. Swift 
    5. PHP
'''

languagePattern = re.compile('(\d)\. (\w*)')

# Returns a list of tuples - each item in the tuple is a captured string
matches = languagePattern.findall(topLanguages)

languageDict = {}

for match in matches:
    position, language = match
    languageDict[position] = language

assert len(languageDict) is 5
assert languageDict['1'] == "Java"
assert languageDict['2'] == "JavaScript"
assert languageDict['3'] == "Python"
assert languageDict['4'] == "Swift"
assert languageDict['5'] == "PHP"
