import java.util.List;

/**
 * Testing different Functor implementations.
 * @author Andrew Jarombek
 * @since 5/25/2019
 */

public class Main {

    /**
     * Generic function which increments the contents of Functor instances
     * @param functor An implementation of Functor that contains on Integer
     * @return A new instance of functor where the contents are incremented by one
     */
    static Functor<Integer> inc(Functor<Integer> functor) {
        return functor.fmap(x -> x + 1);
    }

    public static void main(String... args) {
        // Testing the Functor implementation of a List
        FList<Integer> fList = new FList<>(List.of(1,2,3,4));

        fList = fList.fmap(x -> x * 2)
                .fmap(x -> x + 1);

        List<Integer> list = fList.getList();
        assert list.size() == 4;
        assert list.toString().equals("[3, 5, 7, 9]");

        // Testing the Functor implementation of a Pair
        FPair<Double> fPair = new FPair<>(0.1, 0.2);

        fPair = fPair.fmap(x -> x + 1)
                .fmap(x -> x * 2)
                .fmap(x -> x / 2);

        List<Double> pairList = fPair.getPair();
        assert pairList.size() == 2;
        assert pairList.toString().equals("[1.1, 1.2]");

        // Test generic inc() function
        // inc() works on lists...
        FList<Integer> fList2 = new FList<>(List.of(5,6,7,8));
        fList2 = (FList<Integer>) inc(fList2);

        List<Integer> list2 = fList2.getList();
        assert list2.size() == 4;
        assert list2.toString().equals("[6, 7, 8, 9]");

        // ...and pairs of objects
        FPair<Integer> fPair2 = new FPair<>(1,2);
        fPair2 = (FPair<Integer>) inc(fPair2);

        List<Integer> pairList2 = fPair2.getPair();
        assert pairList2.size() == 2;
        assert pairList2.toString().equals("[2, 3]");
    }
}
