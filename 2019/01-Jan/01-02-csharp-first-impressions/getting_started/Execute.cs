/**
 * A C# program can only have a single main method, so I separated Main into its own file
 * Author: Andrew Jarombek
 * Date: 12/18/2018
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace ConsoleApplication
{
    internal class Execute
    {
        // Same main method as Java
        public static void Main(string[] args)
        {
            // Song.cs
            Console.WriteLine("Begin Testing 'Song'");
            
            var bound2 = new Song("Kanye West", "Bound 2", DateTime.Parse("08/28/2013"), "the entire song");
            
            Debug.Assert(bound2.ToString().Equals("ConsoleApplication.Song: Kanye West - Bound 2"));
            
            Debug.Assert(bound2.Artist.Equals("Kanye West"));
            Debug.Assert(bound2.Name.Equals("Bound 2"));
            Debug.Assert(bound2.BestLyric.Equals("the entire song"));
            
            Console.WriteLine("End Testing 'Song'");
            
            // Run.cs
            Console.WriteLine("Begin Testing 'Run'");

            var timeDict = new Dictionary<string, int>();
            timeDict.Add("minutes", 7);
            timeDict.Add("seconds", 0);
            
            var run = new Run("Easy shaker", 1, timeDict, DateTime.Today);
            Debug.Assert(run.ToString().Equals("ConsoleApplication.Run: Ran 1 in 7:00"));
            
            Console.WriteLine("End Testing 'Run'");
        }
    }
}