/**
 * @author Andrew Jarombek
 * @since 1/15/2018
 */
@FunctionalInterface
public interface CalculateStrategy {
    double execute(double[]... arrs);
}
