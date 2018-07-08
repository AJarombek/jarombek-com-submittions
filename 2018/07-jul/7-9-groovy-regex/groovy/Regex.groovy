#!/usr/bin/env groovy

/**
 * Working with Regular Expressions
 * @author Andrew Jarombek
 * @since 7/8/2018
 */

package groovy

import java.util.regex.Matcher

/* Basic Regex pattern matching and finding for existence */

def datePattern = /\d{1,2}\/\d{1,2}\/\d{4}/

def today = '7/8/2018'
def tomorrow = 'Tomorrow is 7/9/2018.'
def endOfYear = '12/31/2018'
def myBirthday = 'Feb. 26, 2018'

// Test if the entire string matches the pattern
assert today ==~ datePattern
assert !(tomorrow ==~ datePattern)
assert endOfYear ==~ datePattern
assert !(myBirthday ==~ datePattern)

// Test if the pattern exists in the string
assert today =~ datePattern
assert tomorrow =~ datePattern
assert endOfYear =~ datePattern
assert !(myBirthday =~ datePattern)

/* Optimization to split pattern creation time and pattern matching time */

def pattern = ~datePattern

assert pattern.matcher(today).matches()
assert !(tomorrow in pattern)
assert pattern.isCase(endOfYear)
assert !pattern.matcher(myBirthday).matches()

/* Looping through RegEx matches */

def catStatements = '''
    I really like cats.  Cats, cats, CATS!  
    I wish I had a cat, I would name it Cat.
'''

def catRegex = /[Cc][Aa][Tt][Ss]?/

def catAppearances = []

catStatements.eachMatch(catRegex) { match ->
    catAppearances << match
}

assert catAppearances == ['cats', 'Cats', 'cats', 'CATS', 'cat', 'Cat']

/* Looping through Regex Grouping Captures */

def topLanguages = '''
    Top 5 Favorite Programming Languages (as of 7/8/2018)
    1. Java
    2. JavaScript
    3. Python
    4. Swift
    5. PHP
'''

def languagesRegex = /(\w*)/
def numberingRegex = /(\d)\./
def listingRegex = /$numberingRegex $languagesRegex/

// Get all the matches of the regex in the languages string
Matcher languageMatcher = topLanguages =~ listingRegex

def languageMap = [:]

// Loop through each match - the list of regex grouping captures is distributed
// over the parameters
languageMatcher.each { match, num, language ->
    languageMap << [(num):language]
}

assert languageMap == ['1':'Java', '2':'JavaScript', '3':'Python', '4':'Swift', '5':'PHP']