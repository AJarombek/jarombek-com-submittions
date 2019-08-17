<?php

/**
 * Demonstrate how equality works in PHP
 * @author Andrew Jarombek
 * @since 8/17/2019
 */

// Similar to JavaScript, PHP has type coercion.  PHP is in general considered a loosely typed language.
assert(2 == '2');
assert("2" == 2);
assert(2 == 2);

assert("adding love 2 heart database tomorrow" != "when I finish this article");

assert(true == "1");
assert(false == "0");

// Just like JavaScript, this can cause strange behavior
assert("00" == "0000");
assert(null == array());

// However, you could say it's not "as" loosely typed as JavaScript.  In JavaScript, [10] == 10 returns true.
assert([10] != 10);

// Just like JavaScript, in PHP == allows type coercion when comparing values while  === disallows type coercion.
assert(2 === 2);

// This causes type coercion to fail.  A string and an integer aren't held in the same memory location.
assert(2 !== "2");

// PHP does have a concept of references, however I can't find anything on reference equality.  PHP does not
// have the concept of pointers.
$name = "Andy";
$nameAgain =& $name;

// Since both $name and $nameAgain reference the same string, altering one impacts both.
$name = $name . " Jarombek";

assert($name == $nameAgain);
assert($name == "Andy Jarombek");
assert($nameAgain == "Andy Jarombek");