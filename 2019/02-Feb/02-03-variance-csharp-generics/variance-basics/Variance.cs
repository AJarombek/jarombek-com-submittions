/**
 * Work with different forms of variance in C#
 * Author: Andrew Jarombek
 * Date: 2/1/2019
 */

using System.Collections.Generic;
using static System.Diagnostics.Debug;

namespace Variance
{
    class Variance
    {
        static void Main(string[] args)
        {
            // Covariant types
            object myObj1 = new object();
            object myObj2 = "a string literal";
            
            Assert(myObj1 is object);
            Assert(!(myObj1 is string));
            Assert(myObj2 is object);
            Assert(myObj2 is string);
            
            // Generics are invariant
            List<string> list1 = new List<string>();
            
            // Can't convert initializer type to target type
            // List<object> list2 = new List<string>();
        }
    }
}