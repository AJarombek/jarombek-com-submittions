<?php
/**
 * Representation of a Garden I do work in at Tod's Point
 * User: Andrew Jarombek
 * Date: 7/28/2018
 */

class Garden
{

    private $animals;
    private $plants;
    private $randomObjects;

    /**
     * Garden constructor that takes in three lists of items in the garden.
     * These lists default to an empty list if no argument is passed to the constructor.
     * @param array $animals - a list of animals in the garden
     * @param array $plants - a list of plants in the garden
     * @param array $randomObjects - a list of random objects in the garden
     */
    public function __construct($animals=[], $plants=[], $randomObjects=[])
    {
        $this->animals = $animals;
        $this->plants = $plants;
        $this->randomObjects = $randomObjects;
    }

    /**
     * Checks whether a certain item exists in the garden.  Depending on the type of the argument
     * a different internal array will be searched for the item.
     * @param $item - an Animal, Plant, or any other random object
     * @return bool - true if the item exists, false otherwise
     */
    public function inGarden($item) {
        if ($item != null) {

            if ($item instanceof Animal) {
                return in_array($item, $this->animals);
            } else if ($item instanceof Plant) {
                return in_array($item, $this->plants);
            } else {
                return in_array($item, $this->randomObjects);
            }
        } else {
            return false;
        }
    }
}