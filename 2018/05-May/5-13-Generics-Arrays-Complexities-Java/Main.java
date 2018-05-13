import javafx.util.Pair;

import java.util.ArrayList;
import java.util.List;

public class Main {

    public static void main(String... args) {

        // Will Compile and Run, but will throw a java.lang.ArrayStoreException
        Number[] array = new Integer[1];
        // array[0] = 1.6;

        // Wont Compile
        // List<Number> numbers = new ArrayList<Integer>();

        // You can't mix generics and arrays - the following are illegal
        // List<Number>[] arr1 = new ArrayList<>()[10];
        // List<?>[] array = new ArrayList<String>[10];

        List<Number> numberList = new ArrayList<Number>();

        // Java 7 introduced the diamond operator, allowing the
        // compiler to infer the type based on the definition
        List<Number> numberList2 = new ArrayList<>();

        // List<Number> n2 = new ArrayList<Integer>(); Incompatible Types

        // Unbounded wildcard types are reifiable - they enforce their types at runtime.
        // These are the only generics that are reifiable, the rest are all non-reifiable
        List<?> uwList = new ArrayList<>();
        List<?> uwList2 = new ArrayList<Integer>();

        /* Invalid operation */
        // uwList.add("hi");
        // uwList2.add(1);

        List<?> uwList3 = new ArrayList<String>(List.of("hi", "there"));

        /* Invalid operation */
        // String hi = uwList.get(0);

        var hi = uwList3.get(0);
        Object there = uwList3.get(1);

        List<? extends Number> n4 = new ArrayList<Number>();
        List<? extends Number> n5 = new ArrayList<Integer>();
        // List<? extends Number> n6 = new ArrayList<Object>(); Incompatible Types

        List<? super Number> n7 = new ArrayList<Number>();
        // List<? super Number> n8 = new ArrayList<Integer>(); Incompatible Types
        List<? super Number> n8 = new ArrayList<Object>();

        List<? extends Number> numbers = new ArrayList<>(List.of(1,4,6));

        /* Invalid operation */
        // numbers.add(1);

        /* Valid operation */
        Number number = numbers.get(1);
        System.out.println(number);

        System.out.println(numbers.toString());

        List<? super Number> nums = new ArrayList<>(List.of(5, 6, 7));

        /* Valid operation */
        nums.add(8);

        /* Invalid operation */
        // Number num = nums.get(1);

        System.out.println(nums.toString());
    }
}
