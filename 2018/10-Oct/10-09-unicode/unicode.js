/**
 * Testing out JavaScript unicode features
 * @author Andrew Jarombek
 * @since 10/1/2018
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

const delta = '\u0394'; // Δ
assert(delta.length === 1);

// Dealing with combining characters.  The following two strings use different Unicode numbers,
// yet result in strings that appear the same
const b = 'beyonce\u0301'; // beyoncé
const b2 = 'beyonc\u00E9'; // beyoncé

// Both appear to have 7 characters, yet variable 'b' which uses combining characters
// (it combines 'e' and '\u0301', the second of which is a diacritic) is listed
// as having length 8.
assert(b.length === 8);
assert(b2.length === 7);

// While the two strings appear the same, JavaScript does not recognize them as equal
assert(b !== b2);

// Normalize defaults to NFC normalization
const b_norm = b.normalize();
assert(b_norm.length === 7);

// Once normalized, the two strings are equal
assert(b.normalize() === b2.normalize());

// The length of emojis are also greater than one
const smiley = '😊';
assert(smiley.length === 2);

// Find the actual length of an emoji
assert([...smiley].length === 1);

const emojis = '🦌\u{1F98C}';

assert(emojis.length === 4);
assert([...emojis].length === 2);