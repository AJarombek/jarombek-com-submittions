/**
 * Concrete class for an Evergreen tree
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

namespace Multiple_Inheritance
{
    public sealed class EvergreenTree : Tree
    {
        /// <inheritdoc cref="Tree.Type"/>
        public string Type() => "Evergreen";
       
        /// <inheritdoc cref="Tree.Height"/>
        public int Height() => throw new System.NotImplementedException();
        
        // Whether or not the leaves persist in winter
        public bool LeafPersistence => Type() == "Evergreen";
    }
}