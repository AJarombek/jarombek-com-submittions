/**
 * More exploration with C#
 * Author: Andrew Jarombek
 * Date: 12/18/2018
 */

using System;
using System.Collections.Generic;

namespace ConsoleApplication
{
    internal class Run
    {
        public Run(string title, double? distance, Dictionary<string, int> time, DateTime date)
        {
            this.Title = title;
            this.Distance = distance;
            this.Time = time;
            this.Date = date;
        }

        // Shorter property syntax
        public string Title { get; private set; }
        public double? Distance { get; private set; }
        public Dictionary<string, int> Time { get; private set; }
        public DateTime Date { get; private set; }

        private int Minutes
        {
            get { return Time["minutes"]; }
        }
        
        private int Seconds
        {
            get { return Time["seconds"]; }
        }

        public override string ToString()
        {
            var seconds = Seconds < 10 ? "0" + Seconds.ToString() : Seconds.ToString();
            return base.ToString() + ": Ran " + Distance.ToString() + " in " + Minutes.ToString() + ":" + seconds;
        }
    }
}