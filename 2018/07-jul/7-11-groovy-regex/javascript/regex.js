/**
 * Working with Regular Expressions
 * @author Andrew Jarombek
 * @since 7/8/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

// Two regular expression literals in JavaScript.
// The JS engine pre-compiles and caches regex literals before runtime
const datePattern = /\d{1,2}\/\d{1,2}\/\d{4}/;
const exactDatePattern = /^\d{1,2}\/\d{1,2}\/\d{4}$/;

const today = '7/8/2018';
const tomorrow = 'Tomorrow is 7/9/2018.';
const endOfYear = '12/31/2018';
const myBirthday = 'Feb. 26, 2018';

/* Exact matches */
assert(exactDatePattern.test(today));
assert(!exactDatePattern.test(tomorrow));
assert(exactDatePattern.test(endOfYear));
assert(!exactDatePattern.test(myBirthday));

/* Pattern existence match */
assert(datePattern.test(today));
assert(datePattern.test(tomorrow));
assert(datePattern.test(endOfYear));
assert(!datePattern.test(myBirthday));

/* Looping through RegEx matches */

const catStatements = `
    I really like cats.  Cats, cats, CATS!
    I wish I had a cat, I would name it Cat.`;

// The 'g' flag matches all instances of the pattern
const catRegex = /[Cc][Aa][Tt][Ss]?/g;
const catAppearances = catStatements.match(catRegex);

catAppearances.forEach(value => {
   assert(value.toLowerCase().substring(0, 3) === 'cat');
});

assert(catAppearances.toString() === "cats,Cats,cats,CATS,cat,Cat");

/* Looping through Regex Grouping Captures */

const topLanguages = ` Top 5 Favorite Programming Languages (as of 7/8/2018)
    1. Java 2. JavaScript 3. Python 4. Swift 5. PHP`;

const languageRegex = /(\d)\. (\w*)/g;

const languages = {};

let match;
while (match = languageRegex.exec(topLanguages)) {
    languages[`${match[1]}`] = match[2];
}

assert(languages['1'] = "Java");
assert(languages['2'] = "JavaScript");
assert(languages['3'] = "Python");
assert(languages['4'] = "Swift");
assert(languages['5'] = "PHP");