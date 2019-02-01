/**
 * Demonstrate covariance and contravariance with generics
 * Author: Andrew Jarombek
 * Date: 1/15/2019
 */

namespace GenericsArrays
{
    // The input generic type TK supports contravariance.  The output generic type TV supports covariance.
    public interface ICovariant<in TK, out TV>
    {
        /// <summary>
        /// Return an item from a data structure
        /// </summary>
        /// <param name="key">the unique identifier to retrieve a value</param>
        /// <returns>A value of generic type TV</returns>
        TV Get(TK key);
        
        /// <summary>
        /// Remove and return an item from a data structure
        /// </summary>
        /// <param name="key">the unique identifier to retrieve a value</param>
        /// <returns>A value of generic type TV</returns>
        TV Pop(TK key);
    }
}