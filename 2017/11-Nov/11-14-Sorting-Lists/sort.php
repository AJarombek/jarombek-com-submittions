<?php

// Author: Andrew Jarombek
// Date: 11/14/2017
// Sort an array of objects in PHP with a comparator function

class Person 
{
  public $first;
  public $last;
  
  public function __construct($first, $last) 
  {
    $this->first = $first;
    $this->last = $last;
  }
}

$array = array(new Person('Andrew', 'Jar.'), new Person("Thomas", "Cau."),
                new Person("Joe", "Smi."), new Person("Ben", "Fis."));
  
print_r($array);

// Define a comparison function that will be used to sort an array of People by last name 
function comparator($a, $b) {
  return $a->last < $b->last ? -1 : 1; 
}

// Sort with the comparison function
uasort($array, 'comparator');

print_r($array);