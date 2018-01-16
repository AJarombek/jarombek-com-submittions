/**
 * Use the Strategy pattern with a functional interface
 * @author Andrew Jarombek
 * @since 1/15/2018
 */
public class Calculate {
    private CalculateStrategy strategy;

    // On object creation specify which strategy (functional interface) to use
    public Calculate(CalculateStrategy strategy) {
        this.strategy = strategy;
    }

    // Execute the functional interfaces lambda expression implementation
    public double exec(double[]... arrs) {
        return strategy.execute(arrs);
    }
}
