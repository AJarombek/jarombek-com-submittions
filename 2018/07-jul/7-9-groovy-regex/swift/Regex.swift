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

let results: [NSTextCheckingResult]? =
    dateRegex?.matches(in: today, range: NSRange(today.startIndex..., in: today))

results.map {
    print($0)
}
