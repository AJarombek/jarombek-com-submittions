/**
 * Interface for a generic tree
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

namespace Multiple_Inheritance
{
    public interface Tree
    {
        /// <summary>
        /// A string representing the type of tree
        /// </summary>
        /// <returns>The type of tree</returns>
        string Type();
        
        /// <summary>
        /// An integer representing the height of the tree in inches
        /// </summary>
        /// <returns>The height of the tree</returns>
        int Height();
    }
}