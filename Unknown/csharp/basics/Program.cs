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

            // While strings are reference types in C#, testing them for equality uses the value type '==' syntax
            Debug.Assert(name.Item1 == "Andrew" && name.Item2 == "Jarombek");
            
            // ...and tuples with named items
            var info = (name: "Andy", age: 23);
            Debug.Assert(info.name == "Andy" && info.age == 23);

            // You can create an identifier with a keyword if its prefixed with '@'
            var @int = 5;

            Debug.Assert(@int == 5);
            
            // My C# execution config throws an OverflowException by default for number overflows.  Use an 'unchecked'
            // block to change this behavior.
            Debug.Assert(unchecked(int.MaxValue + 1) == int.MinValue);

            // Wrap the value around if a number overflows its max or min value.
            unchecked
            {
                var minValue = int.MaxValue + 1;
                var maxValue = int.MinValue - 1;

                Debug.Assert(maxValue == int.MaxValue && minValue == int.MinValue);
                
                // If you want an OverflowException to be thrown when a number overflows (or a compile time error),
                // use a 'checked' block
                Debug.Assert(checked(5 + 5 == 10));

                // This code won't compile
                // checked(int.MaxValue + 1);
            }

            var i = 0;

            // This 'if' block short circuits and i++ is never called
            if (false && i++ == 1)
            {
                // This code is never reached
                Debug.Assert(false);
            }

            // This 'if' block does not short circuit and i++ is invoked
            if (false & (i++ == 1))
            {
                // This code is never reached
                Debug.Assert(false);
            }

            // Prove that the second if block's boolean comparison did not short circuit
            Debug.Assert(i == 1);

            // Strings prefixed with '@' do not have escape sequences
            var url1 = "https:\\\\jarombek.com";
            var url2 = @"https:\\jarombek.com";
            
            // Strings prefixed with '$' are interpolated strings
            var url3 = $"https:\\\\{name.Item2.ToLower()}.com";
            var url4 = $@"https:\\{name.Item2.ToLower()}.com";

            foreach (var url in new string[] {url1, url2, url3, url4})
            {
                Debug.Assert(url == @"https:\\jarombek.com");
            }

            var multiLine = @"
                Hi there.
                My name is Andy.
            ";
        }
    }
}