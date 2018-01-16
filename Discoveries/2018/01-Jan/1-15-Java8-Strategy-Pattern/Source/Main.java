import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

/**
 * Using Lambda Expressions to Specify an algorithm to execute at runtime
 * @author Andrew Jarombek
 * @since 1/15/2018
 */
public class Main {
    public static void main(String[] args) {
        double[] miles = {2.12, 3.05, 3.05, 2.2, 6.3, 6.5};
        double[] feel = {7,7,6,6,6,6};

        // Calculate the total number of miles run
        Calculate totalMilesCalculator = new Calculate((double[]... array) -> DoubleStream.of(array[0]).sum());
        System.out.println(totalMilesCalculator.exec(miles));

        // Calculate the avg feel
        Calculate avgFeelCalculator = new Calculate((double[]... array) -> DoubleStream.of(array[0]).average().getAsDouble());
        System.out.println(avgFeelCalculator.exec(feel));

        // Calculate the avg feel for each mile run
        Calculate avgFeelByMilesCalculator = new Calculate((double[]... array) -> {
            double totalMiles = DoubleStream.of(array[0]).sum();
            double milesXfeel = IntStream.range(0, array[0].length)
                                    .asDoubleStream()
                                    .map(i -> array[0][(int) i] * array[1][(int) i])
                                    .sum();
            return milesXfeel / totalMiles;
        });
        System.out.println(avgFeelByMilesCalculator.exec(miles, feel));
    }
}
