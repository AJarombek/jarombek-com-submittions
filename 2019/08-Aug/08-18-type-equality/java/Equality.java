/**
 * Determining equality in Java.
 * @author Andrew Jarombek
 * @since 7/27/2019
 */

class Equality {
    static void execute() {
        // Unlike C#, Java primitives can't use equals().  This means they always use value equality.
        int five = 5;
        int six = 6;
        assert five != six;

        int fiveAgain = 5;
        assert five == fiveAgain;

        // Java is strictly typed, casting is necessary to convert types.  There are some exceptions,
        // such as boxing and un-boxing primitives.
        double two = 2.0;
        int twoInt = (int) two;

        // This comparison is successful even though two is of type 'double' and twoInt is of type 'int'.
        assert two == twoInt;

        // Integer is coerced (un-boxed) to primitive type int.  new Integer() is now deprecated.
        assert 2 == new Integer(2);

        Integer twoInteger = 2;
        assert twoInt == twoInteger;

        // In most cases, comparing two values of different types fails at compile time.
        // assert 2 == "2";

        // Java doesn't support operator overloading, so Equals() always checks for value equality and == always checks
        // for reference equality for objects.  I'd argue this is better than C#'s approach.
        String day = "Saturday the 27th";
        String dayAgain = "Saturday the 27th";
        String dayAgainAgain = new String("Saturday the 27th");

        // This is a unique case.  Java caches string literals (not created with a constructor) so that they reference
        // the same underlying object in memory.
        assert day == dayAgain;

        // When Strings are created with a constructor (like dayAgainAgain), they are not cached and reference
        // a new underlying object.
        assert day != dayAgainAgain;

        // equals() does value comparison as expected.
        assert day.equals(dayAgain);
        assert day.equals(dayAgainAgain);

        // Test a custom Yarn object for equality.
        var yarn1 = Yarn.create("Polyester", "Pitter Patter", 210);
        var yarn2 = yarn1;
        var yarn3 = Yarn.create("Polyester", "Pitter Patter", 210);
        var yarn4 = Yarn.create("Unknown", "Vanilla", 70);

        assert yarn1 == yarn2;
        assert yarn1.equals(yarn2);

        assert yarn2 != yarn3;
        assert yarn2.equals(yarn3);

        assert yarn3 != yarn4;
        assert !yarn3.equals(yarn4);
    }
}
