/**
 * Demonstrate overloading and overriding in C#
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

using System;

namespace Overload_Override
{
    class Program
    {
        static void Main(string[] args)
        {
            var msGroundhog = new Animal("Ms. Groundhog", Animal.Species.Groundhog);
            var mrGroundhog = new Animal("Mr. Groundhog", Animal.Species.Groundhog);
            var doe = new Animal("doe", Animal.Species.Deer, "from the tip of his wand burst the silver doe");
            var bunny = new Animal("bunny", Animal.Species.Rabbit);
            
            var hosta = new Plant(Plant.Species.Hosta);
            var lily = new Plant(Plant.Species.Daylily, false);
            var iris = new Plant(Plant.Species.Iris, false);
            
        }
    }
}