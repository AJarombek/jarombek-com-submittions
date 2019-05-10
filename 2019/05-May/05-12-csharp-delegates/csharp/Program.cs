/**
 * Investigate delegates in C#
 * Author: Andrew Jarombek
 * Date: 2/20/2019
 */

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using static System.Diagnostics.Debug;

namespace Delegates
{
    class Program
    {
        /// <summary>
        /// Create a delegate which takes in two integer arguments and returns an integer
        /// </summary>
        delegate int Combiner(int x, int y);

        /// <summary>
        /// Method which is compatible with the Combiner delegate.  Adds two integers.
        /// </summary>
        static int Add(int x, int y) => x + y;
        
        /// <summary>
        /// Method which is compatible with the Combiner delegate.  Subtracts an integer from another integer.
        /// </summary>
        static int Subtract(int x, int y) => x - y;

        /// <summary>
        /// Method which uses the Combiner delegate internally.  Dynamically picks between the Add and Subtract methods
        /// at runtime, depending on a random number value.  This method simulates gambling money.
        /// </summary>
        /// <param name="balance">Your total account balance.</param>
        /// <param name="risking">The amount of money from your balance you are gambling.</param>
        /// <returns>Your new account balance.  Will either increase or decrease by the amount you gamble.</returns>
        static int Gamble(int balance, int risking)
        {
            Random random = new Random();
            int randomNumber = random.Next(0, 10);

            Combiner combiner = (randomNumber >= 5) ? (Combiner) Add : Subtract;

            return combiner(balance, risking);
        }
        
        /// <summary>
        /// Create a delegate which takes in an integer argument, manipulates it, and returns an integer
        /// </summary>
        delegate int Transformer(int x);
        
        /// <summary>
        /// Same as the <code>Transformer</code> delegate except it uses ref returns and ref parameters.
        /// </summary>
        delegate ref int TransformerRef(ref int x);

        /// <summary>
        /// Performs a transformation on every element of the list.  Keeps the original list in-tact, returns a new
        /// list instance.
        /// </summary>
        /// <param name="list">A list of integers</param>
        /// <param name="transformer">A transformation delegate which is given a function at runtime</param>
        /// <returns>A new list instance with newly mapped values</returns>
        static List<int> Map(List<int> list, Transformer transformer) => list.ConvertAll(x => transformer(x));
        
        /// <summary>
        /// Same as <code>Map</code> except it transformations are executed on a reference to an integer.  This causes
        /// the integer to become mutable.  Mutable integers are needed to perform multiple transformations on a memory
        /// location.
        /// </summary>
        static List<int> MapMulti(List<int> list, TransformerRef transformer)
        {
            var newList = new List<int>();
            for (int i = 0; i < list.Count; i++)
            {
                var item = list[i];
                newList.Add(transformer(ref item));
            }

            return newList;
        }

        static void Main(string[] args)
        {
            /* Testing Program.cs */
            
            int newBalance = Gamble(1000, 100);
            
            // Gambling 100 dollars either increases or decreases your balance by 100 dollars
            Assert(newBalance == 1100 || newBalance == 900);
            
            // Create a function to assign to a delegate and a list of integers
            int Doubler(int x) => x * 2;
            var list = new List<int> {5, 10, 15, 20, 25, 31};

            // Call the delegate function on every list item
            var newList = Map(list, Doubler);
            
            // Prove that the new list is mapped to new values
            Assert(newList[0].Equals(10));
            Assert(newList[5].Equals(62));
            
            // Prove that the original list wasn't mutated
            Assert(list[0].Equals(5));
            Assert(list[5].Equals(31));
            
            // C# Delegates have multicast capabilities
            
            // Create a function that increments an integer, using a single memory location
            ref int Increment(ref int x)
            {
                x++;
                return ref x;
            }
            
            // Create another function that triples an integer, using a single memory location
            ref int Triple(ref int x)
            {
                x *= 3;
                return ref x;
            } 
            
            // Create a delegate with two methods.  Increment is called first, Triple is called second
            TransformerRef transformer = Increment;
            transformer += Triple;

            // Prove that integers are transformed in place
            var i = 5;
            transformer(ref i);
                
            Assert(i == 18);
            
            // Prove that list elements containing integers are transformed
            var oddList = new List<int> { 7, 9, 11 };
            var newOddList = MapMulti(oddList, transformer);
            
            Assert(newOddList[0] == 24);
            Assert(newOddList[1] == 30);
            Assert(newOddList[2] == 36);

            // Remove the increment function from the delegate
            transformer -= Increment;

            // Prove that the only transformation executed is Triple.
            var j = 10;
            transformer(ref j);
            
            Assert(j == 30);
            
            /* Testing Util.cs */
            
            var stringList = new List<string> { "Hello", "my", "name", "is", "Andy" };
            var decimalList = new List<decimal> { 3.75m, 4.22m, 2.29m, 3.04m, 4.03m, 6.88m, 3.96m };

            // Prove that Util.Filter works with strings
            bool Capitalized(string str) => Regex.IsMatch(str, "^[A-Z].*");

            var capitalizedWords = Util.Filter(stringList, Capitalized);
            
            Assert(capitalizedWords[0] == "Hello");
            Assert(capitalizedWords[1] == "Andy");
            Assert(capitalizedWords.Count == 2);
            
            // Prove that Util.Filter works with decimals
            bool LongerRun(decimal @decimal) => @decimal > 5.00m;

            var longerRuns = Util.Filter(decimalList, LongerRun);
            
            Assert(longerRuns[0] == 6.88m);
            Assert(longerRuns.Count == 1);
            
            // Prove that Util.Reduce works with strings
            string BuildSentence(string accumulator, string value) => $"{accumulator} {value}";

            var sentence = Util.Reduce(BuildSentence, "Intro:", stringList);
            
            Assert(sentence == "Intro: Hello my name is Andy");
            
            // Prove that Util.Reduce works with decimals
            var weeklyMileage = Util.Reduce((x, y) => x + y, 0m, decimalList);
            
            Assert(weeklyMileage == 28.17m);
        }
    }
}