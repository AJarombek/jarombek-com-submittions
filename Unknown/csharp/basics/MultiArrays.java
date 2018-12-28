/**
 * Demonstrate jagged and rectangular multi-dimensional arrays in Java
 * @author Andrew Jarombek
 * @since 12/28/2018
 */

public class MultiArrays {

    public static void main(String... args) {

        // In Java jagged arrays don't have different syntax from regular arrays
        int[][] jaggedArray = new int[][] {
                {1, 2},
                {3, 4, 5}
        };

        // And rectangular arrays can't be explicitly enforced
        int[][] rectangularArray = new int[2][2];

        rectangularArray[0] = new int[] {1, 2};
        rectangularArray[1] = new int[] {3, 4};

        assert jaggedArray[1][1] == rectangularArray[1][1];
        assert jaggedArray[1][2] == 5;

        // Turning a rectangularArray into a jagged array
        rectangularArray[1] = new int[] {3, 4, 5};

        assert rectangularArray[1][2] == 5;
    }
}
