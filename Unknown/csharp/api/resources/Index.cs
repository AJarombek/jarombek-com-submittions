/**
 * Class with a static constructor, indexer, and finalizer
 * Author: Andrew Jarombek
 * Date: 2/4/2019
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace ClassConcepts
{
    public class Index<T>
    {
        // Private List instance used by the indexer
        private List<T> _list;

        // Public class property set by the static constructor
        public static bool Ready { get; } = false;

        /// <summary>
        /// Static constructors are invoked once per class, right before the class is used at runtime
        /// </summary>
        static Index() => Ready = true;

        /// <summary>
        /// The default constructor simply initializes the internal list
        /// </summary>
        public Index() => _list = new List<T>();

        /// <summary>
        /// This alternate constructor populates the internal list
        /// </summary>
        /// <param name="enumerable">Any collection that implements IEnumerable can be converted to a list</param>
        public Index(IEnumerable<T> enumerable) => _list = enumerable.ToList();

        /// <summary>
        /// Finalizer is invoked by the garbage collector
        /// </summary>
        ~Index() => Console.WriteLine("Index Finalizing");

        /// <summary>
        /// Indexers provide obj[index] syntax for accessing elements
        /// </summary>
        /// <param name="index">The index at which to access or set an element</param>
        public T this[int index]
        {
            get => _list.ElementAt(index);
            set => _list.Insert(index, value);
        }
    }
}