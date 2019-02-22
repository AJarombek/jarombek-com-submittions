/**
 * Investigate C# Delegates in more depth
 * Author: Andrew Jarombek
 * Date: 2/21/2019
 */

using System;
using System.Collections.Generic;

namespace Delegates
{
    public class Util<T> where T: new()
    {
        static T Reduce(List<T> list, Func<T, bool> predicate)
        {
            return new T();
        }
    }
}