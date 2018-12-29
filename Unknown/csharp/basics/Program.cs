/**
 * C# Basics
 * Author: Andrew Jarombek
 * Date: 12/23/2018
 */

using System;
using static System.Diagnostics.Debug;

namespace Basics
{
    internal class Program
    {
        
        /// <summary>
        /// Increment an integer (pass by copy of reference).
        /// </summary>
        /// <param name="num">An integer to incremented by 1</param>
        /// <returns>A new integer</returns>
        static int Inc(int num)
        {
            return num++;
        }

        /// <summary>
        /// Increment an integer (pass by reference)
        /// </summary>
        /// <param name="num">An integer to be incremented by 1</param>
        /// <returns>The passed in integer incremented by 1</returns>
        static int IncRef(ref int num)
        {
            return num++;
        }

        /// <summary>
        /// Information about the class using output parameters
        /// </summary>
        /// <param name="author">Who wrote the class</param>
        /// <param name="date">The date they wrote the class</param>
        static void Info(out string author, out DateTime date)
        {
            author = "Andrew Jarombek";
            date = DateTime.Parse("12/23/2018");
        }
        
        // 'params' keyword allows for a variable number of arguments
        public static void Main(params string[] args)
        {
            // C# has support for tuples
            var name = ("Andrew", "Jarombek");
            Console.WriteLine(name.Item1);
            Console.WriteLine(name.Item2);

            // While strings are reference types in C#, testing them for equality uses the value type '==' syntax
            Assert(name.Item1 == "Andrew" && name.Item2 == "Jarombek");
            
            // ...and tuples with named items
            var info = (name: "Andy", age: 23);
            Assert(info.name == "Andy" && info.age == 23);

            // You can create an identifier with a keyword if its prefixed with '@'
            var @int = 5;

            Assert(@int == 5);
            
            // My C# execution config throws an OverflowException by default for number overflows.  Use an 'unchecked'
            // block to change this behavior.
            Assert(unchecked(int.MaxValue + 1) == int.MinValue);

            // Wrap the value around if a number overflows its max or min value.
            unchecked
            {
                var minValue = int.MaxValue + 1;
                var maxValue = int.MinValue - 1;

                Assert(maxValue == int.MaxValue && minValue == int.MinValue);
                
                // If you want an OverflowException to be thrown when a number overflows (or a compile time error),
                // use a 'checked' block
                Assert(checked(5 + 5 == 10));

                // This code won't compile
                // checked(int.MaxValue + 1);
            }

            var i = 0;

            // This 'if' block short circuits and i++ is never called
            if (false && i++ == 1)
            {
                // This code is never reached
                Assert(false);
            }

            // This 'if' block does not short circuit and i++ is invoked
            if (false & (i++ == 1))
            {
                // This code is never reached
                Assert(false);
            }

            // Prove that the second if block's boolean comparison did not short circuit
            Assert(i == 1);

            // Strings prefixed with '@' do not have escape sequences
            var url1 = "https:\\\\jarombek.com";
            var url2 = @"https:\\jarombek.com";
            
            // Strings prefixed with '$' are interpolated strings
            var url3 = $"https:\\\\{name.Item2.ToLower()}.com";
            var url4 = $@"https:\\{name.Item2.ToLower()}.com";

            foreach (var url in new string[] {url1, url2, url3, url4})
            {
                Assert(url == @"https:\\jarombek.com");
            }

            var multiLine = @"
                Hi there.
                My name is Andy.
            ";
            
            // C# supports both rectangular multidimensional arrays ...
            // (rectangular arrays sizes are strictly enforced)
            int[,] rectangularArray = 
            {
                {1, 2},
                {3, 4}
            };
            
            // and jagged multidimensional arrays
            int[][] jaggedArray = 
            {
                new int[] {1, 2},
                new int[] {3, 4, 5}, 
            };
            
            Assert(!rectangularArray.Equals(jaggedArray));
            Assert(rectangularArray[1,1] == jaggedArray[1][1]);
            Assert(jaggedArray[1][2] == 5);

            var num = 26;
            
            // Inc() doesn't mutate num, and returns the new value
            var num2 = Inc(num);
            
            Assert(num == 26);
            Assert(num2 == 27);
            
            // IncRef() mutates num
            var num3 = IncRef(ref num);
            
            Assert(num == 27);
            Assert(num3 == 27);
            
            // Call a method with output arguments.  Use a discard '_' to ignore certain output arguments.
            Info(out string author, out _);
            
            Assert(author == "Andrew Jarombek");

            int? age = null;

            // Null coalescing operator
            double ageDouble = age ?? 0.0;
            Assert(ageDouble == 0.0);

            // Null conditional operator and null coalescing operator
            string ageStr = age?.ToString() ?? "Unknown";
            Assert(ageStr == "Unknown");

            string username = null;
            
            // Avoid NullReferenceException with null conditional operator
            var upperUsername = username?.ToUpper();
            
            Assert(upperUsername == null);
        }
    }
}