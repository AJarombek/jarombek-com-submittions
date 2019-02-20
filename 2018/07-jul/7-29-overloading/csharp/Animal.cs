/**
 * Representation of an Animal with a specific species and name
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

namespace Overload_Override
{
    // A 'sealed' class restricts inheritance (other classes can't subclass Animal)
    public sealed class Animal
    {
        /// <summary>
        /// The species of animal
        /// </summary>
        public enum Species
        {
            Deer, Groundhog, Rabbit, Raccoon, Squirrel, Chipmunk, Crow, Cardinal
        }
        
        // Animal properties
        public string Name { get; }
        public string Description { get; }
        public Species AnimalSpecies { get; }

        /// <summary>
        /// Construct a new animal with a given name and species.  Optionally, an animal has a description about it.
        /// </summary>
        /// <param name="name">The name of the animal (ex. joe)</param>
        /// <param name="animalSpecies">The species of the animal</param>
        /// <param name="description">A description about the animal</param>
        public Animal(string name, Species animalSpecies, string description = "")
        {
            this.Name = name;
            this.Description = description;
            this.AnimalSpecies = animalSpecies;
        }
    }
}