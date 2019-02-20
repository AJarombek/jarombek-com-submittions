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
        // Garden properties
        public List<Animal> Animals { get; }
        public List<Plant> Plants { get; }
        public List<object> RandomObjects { get; }

        /// <summary>
        /// Construct a new garden.  Each garden contains three collections of objects: animals, plants, and
        /// other random objects
        /// </summary>
        /// <param name="animals">The animals living in the garden.  Throws an error if null</param>
        /// <param name="plants">The plants in the garden.  Throws an error if null</param>
        /// <param name="randomObjects">Other random objects in the garden.  Throws an error if null</param>
        /// <exception cref="ArgumentNullException"></exception>
        public Garden(IEnumerable<Animal> animals, IEnumerable<Plant> plants, IEnumerable<object> randomObjects)
        {
            if (animals == null) throw new ArgumentNullException(nameof(animals));
            if (plants == null) throw new ArgumentNullException(nameof(plants));
            if (randomObjects == null) throw new ArgumentNullException(nameof(randomObjects));

            this.Animals = animals.ToList();
            this.Plants = plants.ToList();
            this.RandomObjects = randomObjects.ToList();
        }

        /// <summary>
        /// Determine if an animal lives in the garden
        /// </summary>
        /// <param name="animal">An animal the look in the garden for</param>
        /// <returns>true if the animal exists in the garden, false otherwise</returns>
        public bool InGarden(Animal animal)
        {
            return Animals.Contains(animal);
        }
        
        /// <summary>
        /// Determine if a plant is in the garden
        /// </summary>
        /// <param name="plant">A plant the look in the garden for</param>
        /// <returns>true if the plant exists in the garden, false otherwise</returns>
        public bool InGarden(Plant plant)
        {
            return Plants.Contains(plant);
        }
        
        /// <summary>
        /// Determine if a random object is in the garden
        /// </summary>
        /// <param name="randomObject">A random object to look in the garden for</param>
        /// <returns>true if the object exists in the garden, false otherwise</returns>
        public bool InGarden(object randomObject)
        {
            return RandomObjects.Contains(randomObject);
        }
    }
}