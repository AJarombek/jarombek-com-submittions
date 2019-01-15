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
        private List<ValueTuple<TK, TV>> contents;
        
        public void Put(TK key, TV value)
        {
            contents.Add((key, value));
        }
        
        /// <inheritdoc cref="ICovariant{TK,TV}.Get"/>
        public TV Get(TK key)
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc cref="ICovariant{TK,TV}.Pop"/>
        public TV Pop(TK key)
        {
            throw new NotImplementedException();
        }
    }
}