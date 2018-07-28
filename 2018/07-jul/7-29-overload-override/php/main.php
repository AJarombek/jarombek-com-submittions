<?php
/**
 * Demonstrate overloading in PHP
 * User: Andrew Jarombek
 * Date: 7/28/2018
 */

include 'Animal.php';
include 'AnimalSpecies.php';
include 'Plant.php';
include 'PlantSpecies.php';
include 'Garden.php';

$msGroundhog = new Animal("Ms. Groundhog", AnimalSpecies::GROUNDHOG,
                            "Enjoys lounging and eating grass all day");
$mrGroundhog = new Animal("Mr. Groundhog", AnimalSpecies::GROUNDHOG);

$doe = new Animal("doe", AnimalSpecies::DEER, "from the tip of his wand burst the silver doe");
$bunny = new Animal("bunny", AnimalSpecies::RABBIT);

$hosta = new Plant(PlantSpecies::HOSTA);
$lily = new Plant(PlantSpecies::DAYLILY);
$iris = new Plant(PlantSpecies::IRIS);

$garden = new Garden([$msGroundhog, $mrGroundhog, $doe, $bunny], [$hosta, $lily, $iris]);

assert($garden->inGarden($doe));
assert($garden->inGarden($lily));
assert(!$garden->inGarden(new Animal("momma rabbit", AnimalSpecies::RABBIT)));