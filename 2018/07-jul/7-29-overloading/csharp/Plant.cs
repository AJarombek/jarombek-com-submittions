/**
 * Representation of a Plant with a specific species and a flag determining whether its in bloom.
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

namespace Overload_Override
{
    public sealed class Plant
    {
        public enum Species
        {
            Hosta, Daylily, Iris, Hibiscus, Peony
        }
        
        public bool InBloom { get; }
        public Species PlantSpecies { get; }

        public Plant(Species plantSpecies, bool inBloom = false)
        {
            this.PlantSpecies = plantSpecies;
            this.InBloom = inBloom;
        }
    }
}