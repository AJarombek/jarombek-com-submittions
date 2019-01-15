/**
 * Demonstrate bit fields in C# with enums
 * Author: Andrew Jarombek
 * Date: 1/15/2019
 */

using System.Collections.Generic;

namespace GenericsArrays
{
    class Program
    {
        static void Main(string[] args)
        {
            // Generics in C# are invariant by default - the compile time generic type
            // must be the same as the runtime type.  The following code fails at compile time:
            // List<object> strings = new List<string>();
        }
    }
}