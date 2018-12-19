<?php
/**
 * Create a trait for a Christmas tree
 * User: Andrew Jarombek
 * Date: 12/17/2018
 */

trait ChristmasTree
{

    public $holidayTree = true;

    // PHP Fatal error:  Trait method type has not been applied, because there are collisions with other trait methods
    // Methods from traits used in a class must be unique.  If multiple traits have methods of the same name,
    // a conflict occurs.

    //public function type() {return "Christmas";}
}