/**
 * Demonstrate multiple inheritance in C#
 * Author: Andrew Jarombek
 * Date: 1/14/2019
 */

using static System.Diagnostics.Debug;

namespace Multiple_Inheritance
{
    class Program
    {
        static void Main(string[] args)
        {
            var balsamFir = new BalsamFir(7, 2);

            Assert(balsamFir.Height() == 86);
            Assert(balsamFir.LeafPersistence);
            Assert(balsamFir.Type() == "Christmas");
        }
    }
}