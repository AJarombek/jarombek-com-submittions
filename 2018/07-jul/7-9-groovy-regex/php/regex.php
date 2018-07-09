<?php

/**
 * Working with Regular Expressions
 * User: Andrew Jarombek
 * Date: 7/8/2018
 */

$datePattern = '/\d{1,2}\/\d{1,2}\/\d{4}/';
$dateExactPattern = '/^\d{1,2}\/\d{1,2}\/\d{4}$/';

$today = '7/8/2018';
$tomorrow = 'Tomorrow is 7/9/2018.';
$endOfYear = '12/31/2018';
$myBirthday = 'Feb. 26, 2018';

// Test if the pattern exists in the string

// preg_match() returns 1 if the pattern matches, 0 otherwise
$todayMatches = preg_match($datePattern, $today);
assert($todayMatches === 1);

$tomorrowMatches = preg_match($datePattern, $tomorrow);
assert($tomorrowMatches === 1);

$endOfYearMatches = preg_match($datePattern, $endOfYear);
assert($endOfYearMatches === 1);

$myBirthdayMatches = preg_match($datePattern, $myBirthday);
assert($myBirthdayMatches === 0);

// Test if the entire string matches the pattern

$todayExactMatch = preg_match($dateExactPattern, $today);
assert($todayExactMatch === 1);

$tomorrowExactMatch = preg_match($dateExactPattern, $tomorrow);
assert($tomorrowExactMatch === 0);

$endOfYearExactMatch = preg_match($dateExactPattern, $endOfYear);
assert($endOfYearExactMatch === 1);

$myBirthdayExactMatch = preg_match($dateExactPattern, $myBirthday);
assert($myBirthdayExactMatch === 0);

// Looping through RegEx matches

$catStatements = '
    I really like cats.  Cats, cats, CATS!  
    I wish I had a cat, I would name it Cat.
';

$catRegex = '/[Cc][Aa][Tt][Ss]?/';

preg_match_all($catRegex, $catStatements, $catMatches, PREG_PATTERN_ORDER);

// The first entry in the matches array is an array of full pattern matches
$cats = $catMatches[0];

assert($cats[0] == 'cats');
assert($cats[3] == 'CATS');
assert($cats[5] == 'Cat');

// Looping through Regex Grouping Captures

$topLanguages = '
Top 5 Favorite Programming Languages (as of 7/8/2018)
    1. Java
    2. JavaScript
    3. Python
    4. Swift
    5. PHP
';

$languageRegex = '/(\d)\. (\w*)/';

preg_match_all($languageRegex, $topLanguages, $languageMatches, PREG_PATTERN_ORDER);

$languages = [];

for ($i = 0; $i <= count($languageMatches) + 1; $i++) {
    $languageRank = $languageMatches[1][$i];
    $languageName = $languageMatches[2][$i];

    $languages[$languageRank] = $languageName;
}

assert($languages[1] == 'Java');
assert($languages[2] == 'JavaScript');
assert($languages[3] == 'Python');
assert($languages[4] == 'Swift');
assert($languages[5] == 'PHP');