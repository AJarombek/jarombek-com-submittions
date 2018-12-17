/**
 * Test multiple inheritance in Java
 * @author Andrew Jarombek
 * @since 12/17/2018
 */

public class Main {
    public static void main(String... args) {
        var balsam = new BalsamFir(7, 2);

        assert balsam.isChristmasTree();
        assert balsam.leafPersistence();
        assert balsam.height().equals("(7, 2)");
    }
}
