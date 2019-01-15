/**
 * Demonstrate generics with invariance in C#
 * Author: Andrew Jarombek
 * Date: 1/15/2019
 */

using System;
using System.Collections.Generic;

namespace GenericsArrays
{
    public class InvariantMap<TK, TV>
    {
        private List<ValueTuple<TK, TV>> contents;

        public InvariantMap(ValueTuple<TK, TV> item)
        {
            contents = new List<(TK, TV)>() {item};
        }

        public void Deconstruct(out List<ValueTuple<TK, TV>> contents)
        {
            contents = this.contents;
        }

        public void Put(TK key, TV value)
        {
            contents.Add((key, value));
        }
    }
}