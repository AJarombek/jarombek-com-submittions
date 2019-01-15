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
        TV Get(TK key);
        TV Pop(TK key);
    }
}