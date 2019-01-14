/**
 * Representation of a Garden containing animals and plants.
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Overload_Override
{
    public class Garden
    {
        public List<Animal> Animals { get; }
        public List<Plant> Plants { get; }
        public List<object> RandomObjects { get; }

        public Garden(IEnumerable<Animal> animals, IEnumerable<Plant> plants, IEnumerable<object> randomObjects)
        {
            if (animals == null) throw new ArgumentNullException(nameof(animals));
            if (plants == null) throw new ArgumentNullException(nameof(plants));
            if (randomObjects == null) throw new ArgumentNullException(nameof(randomObjects));

            this.Animals = animals.ToList();
            this.Plants = plants.ToList();
            this.RandomObjects = randomObjects.ToList();
        }

        public bool InGarden(Animal animal)
        {
            return Animals.Contains(animal);
        }
        
        public bool InGarden(Plant plant)
        {
            return Plants.Contains(plant);
        }
        
        public bool InGarden(object randomObject)
        {
            return RandomObjects.Contains(randomObject);
        }
    }
}