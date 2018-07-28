<?php
/**
 * Class representing an animal of a specific species
 * User: Andrew Jarombek
 * Date: 7/28/2018
 */

class Animal
{
    private $name;
    private $species;
    private $description;

    /**
     * Animal constructor that takes in an optional description.
     * @param $name - a name given to the animal
     * @param $species - the specific species of the animal
     * @param string $description - an optional description of the animal (defaults to an empty string)
     */
    public function __construct($name, $species, $description="")
    {
        $this->name = $name;
        $this->species = $species;
        $this->description = $description;
    }
}