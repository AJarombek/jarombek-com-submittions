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
        public enum Species
        {
            Deer, Groundhog, Rabbit, Raccoon, Squirrel, Chipmunk, Crow, Cardinal
        }
        
        public string Name { get; }
        public string Description { get; }
        public Species AnimalSpecies { get; }

        public Animal(string name, Species animalSpecies, string description = "")
        {
            this.Name = name;
            this.Description = description;
            this.AnimalSpecies = animalSpecies;
        }
    }
}