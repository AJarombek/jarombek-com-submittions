<?php

/**
 * Create a class that represents a ball of yarn.
 * @author Andrew Jarombek
 * @since 8/17/2019
 */

class Yarn
{
    private $fiber;
    private $color;
    private $yards;

    /**
     * Yarn constructor that accepts all necessary fields.
     * @param $fiber - The fiber that the yarn is made of.
     * @param $color - The visual color of the yarn.
     * @param $yards - The length of the yarn in yards.
     */
    public function __construct($fiber, $color, $yards)
    {
        $this->fiber = $fiber;
        $this->color = $color;
        $this->yards = $yards;
    }
}