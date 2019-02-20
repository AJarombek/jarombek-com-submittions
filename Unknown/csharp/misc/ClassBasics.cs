/**
 * Creating classes in C#
 * Author: Andrew Jarombek
 * Date: 1/11/2019
 */

using System;
using System.Collections.Generic;
using static System.Diagnostics.Debug;

namespace Basics
{
    internal class TrailMap
    {
        // 'readonly' prevents a field from being modified after construction
        public readonly string metric;

        /// <summary>
        /// Expression bodied constructor is shorthand for a one line constructor.  Simply initialize the internal
        /// trails list.
        /// </summary>
        public TrailMap() => Trails = new List<ValueTuple<string, double>>();

        /// <summary>
        /// Assign the internal trails list with the parameter passed in.
        /// </summary>
        /// <param name="trails">A list of tuples containing trail information.</param>
        public TrailMap(List<ValueTuple<string, double>> trails) => this.Trails = trails;

        /// <inheritdoc />
        /// <summary>
        /// Assign the internal location of the trails to the location parameter.  Delegate the assignment of the
        /// trail list to the previous constructor.
        /// </summary>
        /// <param name="trails">A list of tuples containing trail information.</param>
        /// <param name="location">The location of the trails.</param>
        private TrailMap(List<ValueTuple<string, double>> trails, string location) : 
            this(trails) => this.Location = location;
        
        /// <inheritdoc />
        /// <summary>
        /// Same as previous constructor except with an optional "Metric" argument.
        /// </summary>
        /// <param name="trails">A list of tuples containing trail information.</param>
        /// <param name="location">The location of the trails.</param>
        /// <param name="metric">
        /// An optional parameter containing the metric for the trail length.  Defaults to "Miles"
        /// </param>
        public TrailMap(List<ValueTuple<string, double>> trails, string location, string metric="Miles") : 
            this(trails, location) => this.metric = metric;

        /// <summary>
        /// Deconstruct the TrailMap object.  While a constructor takes values and assigns them to fields, a
        /// deconstructor takes fields and returns their values.
        /// </summary>
        /// <param name="trails">out parameter representing a list of trails</param>
        /// <param name="location">out parameter representing the location of the trails</param>
        public void Deconstruct(out List<ValueTuple<string, double>> trails, out string location)
        {
            trails = Trails;
            location = Location;
        }

        // Trails property
        public List<ValueTuple<string, double>> Trails { get; }

        // Location property with an initial value of "None"
        public string Location { get; } = "None";

        // Count property
        public int Count => Trails.Count;
    }

    internal class Program
    {
        public static void Main(string[] args)
        {
            var trailList = new List<ValueTuple<string, double>>()
            {
                ("Main Road", 5.6), ("Swamp Trail", 3.0), ("Cave Trail", 1.5)
            };
            
            var trailMap = new TrailMap(trailList, "Mianus River Park");
            
            // Invoke the deconstructor
            var (_, location) = trailMap;

            Assert(trailMap.Location == location);
            Assert(trailMap.Count == 3);
            Assert(trailMap.metric == "Miles");
        }
    }
}