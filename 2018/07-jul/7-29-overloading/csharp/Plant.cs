/**
 * Representation of a Plant with a specific species and a flag determining whether its in bloom.
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

namespace Overload_Override
{
    public sealed class Plant
    {
        /// <summary>
        /// The species of plant
        /// </summary>
        public enum Species
        {
            Hosta, Daylily, Iris, Hibiscus, Peony
        }
        
        // Plant properties
        public bool InBloom { get; }
        public Species PlantSpecies { get; }

        /// <summary>
        /// Construct a new plant of a given species.  The plant defaults to not being in bloom.
        /// </summary>
        /// <param name="plantSpecies">The species of the plant</param>
        /// <param name="inBloom">Whether or not the plant is in bloom.  This parameter is optional</param>
        public Plant(Species plantSpecies, bool inBloom = false)
        {
            this.PlantSpecies = plantSpecies;
            this.InBloom = inBloom;
        }
    }
}