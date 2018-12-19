<?php
/**
 * Test out using Traits for 'multiple inheritance' in PHP
 * User: Andrew Jarombek
 * Date: 12/18/2018
 */

include 'Tree.php';
include 'ChristmasTree.php';
include 'EvergreenTree.php';
include 'BalsamFir.php';

$tree_height = ["feet" => 7, "inches" => 2];

$balsam = new BalsamFir($tree_height);

print_r($balsam);

assert($balsam->type() == "Evergreen");
assert($balsam->height() == "7' 2\"");
assert($balsam->holidayTree);
