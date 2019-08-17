/**
 * Investigate basic C# class concepts which will help creating an API
 * Author: Andrew Jarombek
 * Date: 2/4/2019
 */

using System;
using static System.Diagnostics.Debug;

namespace ClassConcepts
{
    class Program
    {
        static void Main(string[] args)
        {
            /* Testing Index.cs */
            
            // Prove the static constructor is invoked just prior to the type being used.
            Assert(Index<int>.Ready);
            Assert(Index<object>.Ready);
            
            var indexCollection = new Index<string>();
            indexCollection[0] = "Andy";
            
            Assert(indexCollection[0].Equals("Andy"));

            try
            {
                indexCollection[2] = "Tom";
                
                // Should not reach this line
                Assert(false);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error: Must insert at a valid index.");
            }
            
        }
    }
}