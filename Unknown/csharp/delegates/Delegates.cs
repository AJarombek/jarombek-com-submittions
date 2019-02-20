/**
 * Investigate delegates in C#
 * Author: Andrew Jarombek
 * Date: 2/20/2019
 */

using System;
using System.Collections.Generic;
using System.Security.Cryptography.X509Certificates;
using static System.Diagnostics.Debug;

namespace Delegates
{
    class Delegates
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
        /// Performs a transformation on every element of the list.  Keeps the original list in-tact, returns a new
        /// list instance.
        /// </summary>
        /// <param name="list">A list of integers</param>
        /// <param name="transformer">A transformation delegate which is given a function at runtime</param>
        /// <returns>A new list instance with newly mapped values</returns>
        static List<int> Map(List<int> list, Transformer transformer) => list.ConvertAll(x => transformer(x));
        
        static void Main(string[] args)
        {
            int newBalance = Gamble(1000, 100);
            
            // Gambling 100 dollars either increases or decreases your balance by 100 dollars
            Assert(newBalance == 1100 || newBalance == 900);
            
            // Create a delegate function and a list of integers
            Transformer doubler = x => x * 2;
            var list = new List<int> {5, 10, 15, 20, 25, 31};

            // Call the delegate function on every list item
            var newList = Map(list, doubler);
            
            // Prove that the new list is mapped to new values
            Assert(newList[0].Equals(10));
            Assert(newList[5].Equals(62));
            
            // Prove that the original list wasn't mutated
            Assert(list[0].Equals(5));
            Assert(list[5].Equals(31));
        }
    }
}