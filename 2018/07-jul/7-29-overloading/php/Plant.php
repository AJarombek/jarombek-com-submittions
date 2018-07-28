<?php
/**
 * Class representing a plant of a specific species
 * User: Andrew Jarombek
 * Date: 7/28/2018
 */

class Plant
{
    private $species;
    private $inBloom;

    /**
     * Plant constructor that takes in an optional flag determining whether or not the plants
     * flower is in bloom.
     * @param $species - the specific species of plant
     * @param bool $inBloom - whether or not the plant is in bloom
     */
    public function __construct($species, $inBloom=false)
    {
        $this->species = $species;
        $this->inBloom = $inBloom;
    }
}