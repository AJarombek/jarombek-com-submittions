/**
 * Concrete class for a Balsam Fir which implements Tree and is a composition of ChristmasTree and EvergreenTree.
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

namespace Multiple_Inheritance
{
    public sealed class BalsamFir : Tree
    {
        // private immutable fields used for composition
        private readonly ChristmasTree _christmasTree = new ChristmasTree();
        private readonly EvergreenTree _evergreenTree = new EvergreenTree();
        
        // BalsamFir properties
        private int Feet { get; }
        private int Inches { get; }

        /// <summary>
        /// Construct a balsam fir tree
        /// </summary>
        /// <param name="feet">The height of the tree in feet (floored)</param>
        /// <param name="inches">The height of the tree in inches</param>
        public BalsamFir(int feet, int inches)
        {
            Feet = feet;
            Inches = inches;
        }

        /// <inheritdoc cref="Tree.Type"/>
        public string Type() => _christmasTree.Type();

        /// <inheritdoc cref="Tree.Height"/>
        public int Height() => Feet * 12 + Inches;

        // Whether or not the leaves persist in winter
        public bool LeafPersistence => _evergreenTree.LeafPersistence;
    }
}