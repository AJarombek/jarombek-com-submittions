<?php
/**
 * Create a class for a balsam fir tree, which is both a christmas tree and an evergreen tree
 * User: Andrew Jarombek
 * Date: 12/17/2018
 */

class BalsamFir implements Tree
{
    // Add traits to the class
    use EvergreenTree;
    use ChristmasTree;

    private $height_feet;
    private $height_inches;

    public function __construct($height)
    {
        $this->height_feet = $height["feet"];
        $this->height_inches = $height["inches"];
    }

    public function height()
    {
        return sprintf("%d' %d\"", $this->height_feet, $this->height_inches);
    }
}