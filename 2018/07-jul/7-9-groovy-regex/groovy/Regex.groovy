#!/usr/bin/env groovy

/**
 * Working with Regular Expressions
 * @author Andrew Jarombek
 * @since 7/8/2018
 */

package groovy

/* Basic Regex pattern matching and finding for existence */

def datePattern = /\d{1,2}\/\d{1,2}\/\d{4}/

def today = '7/8/2018'
def tomorrow = 'Tomorrow is 7/9/2018.'
def endOfYear = '12/31/2018'
def myBirthday = 'Feb. 26, 2018'

/* Test if the entire string matches the pattern */
assert today ==~ datePattern
assert endOfYear ==~ datePattern
assert !(tomorrow ==~ datePattern)
assert !(myBirthday ==~ datePattern)

/* Test if the pattern exists in the string */
assert today =~ datePattern
assert endOfYear =~ datePattern
assert tomorrow =~ datePattern
assert !(myBirthday =~ datePattern)