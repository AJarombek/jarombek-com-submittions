/**
 Working with Regular Expressions
 - Author: Andrew Jarombek
 - Date: 7/8/2018
 */

import Foundation

/**
 Search a String for matches to a Regular Expression
 - parameters:
    - string: A string object to search
    - regex: A string formatted as a regular expression
 - Returns: true if the regular expression matches somewhere in the string, false otherwise
 */
func match(_ string: String, withRegex regex: String) -> Bool {
    do {
        let regExp: NSRegularExpression = try NSRegularExpression(pattern: regex)
        return match(string, withRegex: regExp)
    } catch {
        return false
    }
}

/**
 Search a String for matches to a Regular Expression
 - parameters:
    - string: A string object to search
    - regex: An optional regular expression.  If the regular expression is nil, the function will return false
 - Returns: true if the regular expression matches somewhere in the string, false otherwise
 */
func match(_ string: String, withRegex regex: NSRegularExpression?) -> Bool {
    if let match: NSTextCheckingResult =
        regex?.firstMatch(in: string, options: [], range: NSRange(string.startIndex..., in: string)) {

        match.range(at: 0)
        return true
    } else {
        return false
    }
}

/**
 Search a String for multiple matches to a Regular Expression
 - parameters:
 - string: A string object to search
 - regex: A string formatted as a regular expression
 - Returns: an optional array of regular expression matching results
 */
func getMatches(_ string: String, withRegex regex: String) -> [NSTextCheckingResult]? {
    do {
        let regExp: NSRegularExpression = try NSRegularExpression(pattern: regex)
        return getMatches(string, withRegex: regExp)
    } catch {
        return nil
    }
}

/**
 Search a String for multiple matches to a Regular Expression
 - parameters:
 - string: A string object to search
 - regex: An optional regular expression.  If the regular expression is nil, return nil
 - Returns: an optional array of regular expression matching results
 */
func getMatches(_ string: String, withRegex regex: NSRegularExpression?) -> [NSTextCheckingResult]? {
    return regex?.matches(in: string, range: NSRange(string.startIndex..., in: string))
}

/**
 Check if an entire string matches a regular expression.  The range of the string is compared to the range of its
 substring that matches the regular expression pattern.  If both ranges are the same, the entire string matched
 the regular expression pattern.
 - parameters:
    - string: A string object to try and match the pattern
    - regex: A string formatted as a regular expression
 - Returns: true if the entire string matches the regular expression, false otherwise
 */
func fullMatch(of string: String, withRegex regex: String) -> Bool {
    let rangeOfString: Range = string.startIndex..<string.endIndex
    if let rangeMatched: Range = string.range(of: regex, options: .regularExpression) {
        return rangeOfString == rangeMatched
    } else {
        return false
    }
}

let today = "7/8/2018"
let tomorrow = "Tomorrow is 7/9/2018."
let endOfYear = "12/31/2018"
let myBirthday = "Feb. 26, 2018"

let datePattern: String = "\\d{1,2}\\/\\d{1,2}\\/\\d{4}"
let dateRegex: NSRegularExpression? = try NSRegularExpression(pattern: datePattern)

// Exact matches
let todayExactMatches: Bool = fullMatch(of: today, withRegex: datePattern)
let tomorrowExactMatches: Bool = fullMatch(of: tomorrow, withRegex: datePattern)
let endOfYearExactMatches: Bool = fullMatch(of: endOfYear, withRegex: datePattern)
let myBirthdayExactMatches: Bool = fullMatch(of: myBirthday, withRegex: datePattern)

assert(todayExactMatches)
assert(!tomorrowExactMatches)
assert(endOfYearExactMatches)
assert(!myBirthdayExactMatches)

// Pattern existance match
let todayMatches: Bool = match(today, withRegex: dateRegex)
let tomorrowMatches: Bool = match(tomorrow, withRegex: dateRegex)
let endOfYearMatches: Bool = match(endOfYear, withRegex: dateRegex)
let myBirthdayMatches: Bool = match(myBirthday, withRegex: dateRegex)

assert(todayMatches)
assert(tomorrowMatches)
assert(endOfYearMatches)
assert(!myBirthdayMatches)

// Looping through RegEx matches

let catStatements = """
    I really like cats.  Cats, cats, CATS!
    I wish I had a cat, I would name it Cat.
"""

let catPattern = "[Cc][Aa][Tt][Ss]?"
let catRegex: NSRegularExpression? = try NSRegularExpression(pattern: catPattern)

if let results: [NSTextCheckingResult] = getMatches(catStatements, withRegex: catRegex) {

    var cats: Array<String> = []

    results.map {
        let range: NSRange = $0.range(at: 0)
        let start: String.Index = String.Index(encodedOffset: range.lowerBound)
        let end: String.Index = String.Index(encodedOffset: range.upperBound)
        let cat: Substring = catStatements[start..<end]
        cats.append(String(cat))
    }

    assert(cats.count == 6)
    assert(cats[0] == "cats")
    assert(cats[3] == "CATS")
    assert(cats[5] == "Cat")

} else {
    assert(false)
}

// Looping through Regex Grouping Captures

let topLanguages = """
Top 5 Favorite Programming Languages (as of 7/8/2018)
    1. Java
    2. JavaScript
    3. Python
    4. Swift
    5. PHP
"""

let languagePattern = "(\\d)\\. (\\w*)"

if let results: [NSTextCheckingResult] = getMatches(topLanguages, withRegex: languagePattern) {

    var languages: [String:String] = [:]

    results.map {
        let rangeFirstGroup: NSRange = $0.range(at: 1)
        let startFirstGroup: String.Index = String.Index(encodedOffset: rangeFirstGroup.lowerBound)
        let endFirstGroup: String.Index = String.Index(encodedOffset: rangeFirstGroup.upperBound)
        let firstGroup: Substring = topLanguages[startFirstGroup..<endFirstGroup]

        let rangeSecondGroup: NSRange = $0.range(at: 2)
        let startSecondGroup: String.Index = String.Index(encodedOffset: rangeSecondGroup.lowerBound)
        let endSecondGroup: String.Index = String.Index(encodedOffset: rangeSecondGroup.upperBound)
        let secondGroup: Substring = topLanguages[startSecondGroup..<endSecondGroup]
        languages[String(firstGroup)] = String(secondGroup)
    }

    assert(languages["1"] == "Java")
    assert(languages["2"] == "JavaScript")
    assert(languages["3"] == "Python")
    assert(languages["4"] == "Swift")
    assert(languages["5"] == "PHP")

} else {
    assert(false)
}
