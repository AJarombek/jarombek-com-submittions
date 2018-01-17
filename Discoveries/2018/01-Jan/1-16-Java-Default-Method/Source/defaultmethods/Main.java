package defaultmethods;

/**
 * @author Andrew Jarombek
 * @since 1/16/2018
 */
public class Main {

    public static void main(String... args) {
        Cat snickers = new Cat("Snickers", "Caroline D");

        int age = snickers.age();
        System.out.println(age);

        String status = snickers.status();
        System.out.println(status);
    }
}
