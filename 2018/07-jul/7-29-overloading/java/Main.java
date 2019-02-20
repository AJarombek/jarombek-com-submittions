import java.util.List;

/**
 * Testing the Garden API to showcase overloaded methods
 * @author Andrew Jarombek
 * @since 7/22/2018
 */
public class Main {

    public static void main(String... args) {

        Animal msGroundhog = Animal.ofName("Ms. Groundhog", Animal.Species.GROUNDHOG,
                "Enjoys lounging and eating grass all day");
        Animal mrGroundhog = Animal.ofName("Mr. Groundhog", Animal.Species.GROUNDHOG);

        Animal doe = Animal.ofName("doe", Animal.Species.DEER,
                "from the tip of his wand burst the silver doe");

        Animal bunny = Animal.ofName("bunny", Animal.Species.RABBIT);

        Plant hosta = Plant.ofSpecies(Plant.Species.HOSTA);
        Plant lily = Plant.ofSpecies(Plant.Species.DAYLILY, true);
        Plant iris = Plant.ofSpecies(Plant.Species.IRIS, false);

        Garden garden = Garden.of(List.of(msGroundhog, mrGroundhog, doe, bunny), List.of(hosta, lily, iris));

        /* The compile time type of 'doe' is Animal, so inGarden(Animal) is invoked */
        assert garden.inGarden(doe);

        Object objectDoe = doe;

        /* The compile time type of 'objectDoe' is Object, so inGarden(Object) is invoked */
        assert !garden.inGarden(objectDoe);
    }
}
