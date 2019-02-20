/**
 * Concrete class for a Christmas tree
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

namespace Multiple_Inheritance
{
    public sealed class ChristmasTree : Tree
    {
        /// <inheritdoc cref="Tree.Type"/>
        public string Type() => "Christmas";

        /// <inheritdoc cref="Tree.Height"/>
        public int Height() => throw new System.NotImplementedException();
    }
}