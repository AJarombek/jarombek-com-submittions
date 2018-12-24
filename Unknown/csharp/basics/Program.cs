/**
 * C# Basics
 * Author: Andrew Jarombek
 * Date: 12/23/2018
 */

using System;
using System.Diagnostics;

namespace Basics
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            // C# has support for tuples
            var name = ("Andrew", "Jarombek");
            Console.WriteLine(name.Item1);
            Console.WriteLine(name.Item2);

            Debug.Assert(name.Item1 == "Andrew" && name.Item2 == "Jarombek");
            
            // ...and tuples with named items
            var info = (name: "Andy", age: 23);
            Debug.Assert(info.name == "Andy" && info.age == 23);

            // You can create an identifier with a keyword if its prefixed with '@'
            var @int = 5;

            Debug.Assert(@int == 5);
        }
    }
}