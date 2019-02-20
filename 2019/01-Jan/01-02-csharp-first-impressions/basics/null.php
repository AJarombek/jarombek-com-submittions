<?php
/**
 * Test out null operators in PHP - PHP has a null-coalescing operator but NOT a null-conditional operator
 * User: Andrew Jarombek
 * Date: 12/29/2018
 */

$ageOptional = null;

// Null-coalescing operator
$age = $ageOptional ?? -1;

assert($age == -1);