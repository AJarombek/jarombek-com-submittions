/**
 * Investigate C# Delegates in more depth
 * Author: Andrew Jarombek
 * Date: 2/21/2019
 */

using System;
using System.Collections.Generic;

namespace Delegates
{
    public class Util
    {
        /// <summary>
        /// Filter the items in a list based on a predicate
        /// </summary>
        /// <param name="list">A generic list</param>
        /// <param name="predicate">A predicate function on parameter T</param>
        /// <returns>A new instance of List containing items that passed the predicate test</returns>
        public static List<T> Filter<T>(List<T> list, Func<T, bool> predicate)
        {
            var newList = new List<T>();
            foreach (var item in list)
            {
                if (predicate(item))
                {
                    newList.Add(item);
                }
            }

            return newList;
        }

        /// <summary>
        /// Reduce the items in a list into a single value
        /// </summary>
        /// <param name="func">A function which takes an item from the list, an initial value,
        /// and returns a new value</param>
        /// <param name="initialValue">The initial value used to accumulate the single return value</param>
        /// <param name="list">A generic list</param>
        /// <returns>A single value that matches the type of the list contents</returns>
        public static T Reduce<T>(Func<T, T, T> func, T initialValue, List<T> list)
        {
            var result = initialValue;
            foreach (var item in list)
            {
                result = func(result, item);
            }

            return result;
        }
    }
}