/**
 * Demonstrate bit fields in C# with enums
 * Author: Andrew Jarombek
 * Date: 1/15/2019
 */

using System;
using static System.Diagnostics.Debug;

namespace BitFields
{
    class BitField
    {
        
        /// <summary>
        /// The [Flags] attribute should always be used when the members of an enum are combinable.
        /// Combinable enums can be used as a bit field.
        /// </summary>
        [Flags]
        private enum User
        {
            // Different statuses of a user
            None = 0,
            Validated = 1,
            Subscribed = 2,
            Admin = 4,
            
            // Combination members can be included in the enum declaration
            ValidatedAdmin = Validated | Admin,
            SubscribedAdmin = Subscribed | Admin,
            ValidatedSubscribed = Validated | Subscribed,
            All = ValidatedSubscribed | Admin
        }

        /// <summary>
        /// Add new fields to an enum using a bit mask
        /// </summary>
        /// <param name="target">A user enum to apply a bit mask to</param>
        /// <param name="mask">A bit mask to apply to the bit field (User enum)</param>
        /// <returns>A new User enum</returns>
        private static User Add(User target, User mask)
        {
            return target | mask;
        }

        /// <summary>
        /// Toggle fields on and off in an enum using a bit mask
        /// </summary>
        /// <param name="target">A user enum to apply a bit mask to</param>
        /// <param name="mask">A bit mask to apply to the bit field (User enum)</param>
        /// <returns>A new User enum</returns>
        private static User Toggle(User target, User mask)
        {
            return target ^ mask;
        }

        /// <summary>
        /// Remove fields from an enum using a bit mask
        /// </summary>
        /// <param name="target">A user enum to apply a bit mask to</param>
        /// <param name="mask">A bit mask to apply to the bit field (User enum)</param>
        /// <returns>A new User enum</returns>
        private static User Remove(User target, User mask)
        {
            return target & ~mask;
        }

        /// <summary>
        /// Check if fields exist in an enum using a bit mask
        /// </summary>
        /// <param name="target">A user enum to apply a bit mask to</param>
        /// <param name="mask">A bit mask to apply to the bit field (User enum)</param>
        /// <returns>true if the bit field (User enum) contains the mask, false otherwise</returns>
        private static bool Contains(User target, User mask)
        {
            return (target & mask) > 0;
        }
        
        static void Main(string[] args)
        {
            const User user = BitField.User.None;
            
            // Adding the Admin field to the None field creates an admin
            Assert(BitField.Add(user, User.Admin) == User.Admin);
            
            // Adding the Subscribed field to the None field creates a subscribed user.  This also proves that 
            // add() doesn't mutate the 'user' value.
            Assert(BitField.Add(user, User.Subscribed) == User.Subscribed);

            const User user2 = BitField.User.ValidatedSubscribed;
            
            // Prove that an enum with combined flags contains both flags individually.
            Assert(BitField.Contains(user2, User.Validated));
            Assert(BitField.Contains(user2, User.Subscribed));
            
            Assert(BitField.Remove(user2, User.Subscribed) == User.Validated);
            
            // Toggle the Admin field on, then toggle the Validated field off
            Assert(BitField.Toggle(user2, User.Admin) == User.All);
            Assert(BitField.Toggle(user2, User.Validated) == User.Subscribed);
        }
    }
}