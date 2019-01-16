/**
 * Demonstrate generics with variance in C#
 * Author: Andrew Jarombek
 * Date: 1/15/2019
 */

using System;
using System.Collections.Generic;

namespace GenericsArrays
{
    public class VariantMap<TK, TV> : ICovariant<TK, TV>
    {
        // Internal contents of the map
        private readonly List<ValueTuple<TK, TV>> _contents;
        
        /// <summary>
        /// Default constructor which initializes the internal contents
        /// </summary>
        public VariantMap() => _contents = new List<(TK, TV)>();

        /// <summary>
        /// Construct a new VariantMap with an initial key and value.
        /// </summary>
        /// <param name="item">An initial tuple to put in the map</param>
        public VariantMap(ValueTuple<TK, TV> item) => _contents = new List<(TK, TV)> {item};
        
        /// <summary>
        /// Deconstruct the VariantMap object
        /// </summary>
        /// <param name="content">a list of tuples that exist in the map</param>
        /// <param name="length">the number of items in the map</param>
        public void Deconstruct(out List<ValueTuple<TK, TV>> content, out int length)
        {
            content = this._contents;
            length = this.Length;
        }
        
        // Property that calculates the length of the map
        public int Length => _contents.Count;


        /// <inheritdoc cref="object.ToString"/>
        public override string ToString()
        {
            return _contents.ToString();
        }
        
        /// <summary>
        /// Put a new key -> value pair into the map
        /// </summary>
        /// <param name="key">The unique identifier in the map</param>
        /// <param name="value">The value accessed with the key</param>
        public void Put(TK key, TV value)
        {
            _contents.Add((key, value));
        }
        
        /// <inheritdoc cref="ICovariant{TK,TV}.Get"/>
        public TV Get(TK key)
        {
            var tup = _contents.Find(x => x.Item1.Equals(key));
            return tup.Item2;
        }

        /// <inheritdoc cref="ICovariant{TK,TV}.Pop"/>
        public TV Pop(TK key)
        {
            var tup = _contents.Find(x => x.Item1.Equals(key));
            _contents.Remove(tup);
            return tup.Item2;
        }
    }
}