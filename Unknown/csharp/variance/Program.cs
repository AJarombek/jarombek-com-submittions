/**
 * Demonstrate bit fields in C# with enums
 * Author: Andrew Jarombek
 * Date: 1/15/2019
 */

using static System.Diagnostics.Debug;

namespace GenericsArrays
{
    class Program
    {
        static void Main(string[] args)
        {
            // Generics in C# are invariant by default - the compile time generic type
            // must be the same as the runtime type.  The following code fails at compile time:
            // List<object> strings = new List<string>();
            
            // Map that doesn't use variance between compile time and runtime generic types
            InvariantMap<string, int> invariantMap = new InvariantMap<string, int>(("Andy", 226));
            invariantMap.Put("Tom", 501);

            // InvariantMap deconstruction
            var (content, _) = invariantMap;
            
            Assert(content[1].Equals(("Tom", 501)));
            Assert(invariantMap.Get("Andy") == 226);
            
            Assert(invariantMap.Length == 2);

            invariantMap.Pop("Tom");
            Assert(invariantMap.Length == 1);
            
            // Unable to use variance
            // InvariantMap<string, IComparable> invariantMap2 = new InvariantMap<object, Int64>();
            
            ICovariant<string, object> variantMap = new VariantMap<object, string>();
        }
    }
}