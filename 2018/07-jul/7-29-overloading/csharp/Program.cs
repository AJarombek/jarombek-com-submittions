/**
 * Demonstrate overloading and overriding in C#
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

using System.Collections.Generic;
using static System.Diagnostics.Debug;

namespace Overload_Override
{
    class Program
    {
        static void Main(string[] args)
        {
            // Construct the animals in the garden
            var msGroundhog = new Animal("Ms. Groundhog", Animal.Species.Groundhog);
            var mrGroundhog = new Animal("Mr. Groundhog", Animal.Species.Groundhog);
            var doe = new Animal("doe", Animal.Species.Deer, "from the tip of his wand burst the silver doe");
            var bunny = new Animal("bunny", Animal.Species.Rabbit);

            var animals = new List<Animal>() { msGroundhog, mrGroundhog, doe, bunny };

            // Construct the plants in the garden
            var hosta = new Plant(Plant.Species.Hosta);
            var lily = new Plant(Plant.Species.Daylily, false);
            var iris = new Plant(Plant.Species.Iris, false);
            
            var plants = new List<Plant>() { hosta, lily, iris };
            
            var garden = new Garden(animals, plants, new List<object>());

            // The compile time type of 'doe' is Animal, so inGarden(Animal) is invoked
            var doeInGarden = garden.InGarden(doe);
            Assert(doeInGarden);
            
            object objectDoe = doe;
            
            // The compile time type of 'objectDoe' is object, so inGarden(object) is invoked
            doeInGarden = garden.InGarden(objectDoe);
            Assert(!doeInGarden);

            // The runtime type of 'objectDoe' is Animal, so inGarden(Animal) is invoked.  The runtime type is invoked 
            // because objectDoe is cast to 'dynamic', which changes C# static typing behavior to dynamic typing.
            doeInGarden = garden.InGarden((dynamic) objectDoe);
            Assert(doeInGarden);
        }
    }
}