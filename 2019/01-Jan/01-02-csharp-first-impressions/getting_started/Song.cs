/**
 * Beginning with C#
 * Author: Andrew Jarombek
 * Date: 12/17/2018
 */

// Include the System namespace in the program
using System;
using System.Diagnostics;

// A namespace is a collection of classes
namespace ConsoleApplication
{
    // internal access modifier makes a class only accessible from the same assembly
    // The assembly configuration is found in AssemblyInfo.cs
    internal class Song
    {
        private string artist;
        private string name;
        private DateTime releaseDate;
        private string bestLyric;

        public Song(string artist, string name, DateTime releaseDate, string bestLyric)
        {
            this.artist = artist;
            this.name = name;
            this.releaseDate = releaseDate;
            this.bestLyric = bestLyric;
        }
        
        // Override Object.ToString()
        public override string ToString()
        {
            return base.ToString() + ": " + artist + " - " + name;
        }

        // Getters for the private properties of Song
        public string Artist
        {
            get { return artist; }
        }

        public string Name
        {
            get { return name; }
        }

        public DateTime ReleaseDate
        {
            get { return releaseDate; }
        }

        public string BestLyric
        {
            get { return bestLyric; }
        }
    }
}