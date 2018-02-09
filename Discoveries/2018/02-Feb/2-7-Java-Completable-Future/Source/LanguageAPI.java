package completablefuture;

import java.time.LocalDate;
import java.time.Month;
import java.util.Map;
import java.util.Random;
import static java.util.stream.Collectors.toMap;

import java.util.stream.Stream;

/**
 * The Language API that simulates network call delays
 * @author Andrew Jarombek
 * @since 2/7/2018
 */
public class LanguageAPI {

    private static Random random =  new Random();
    private Map<String, Language> languages;

    public LanguageAPI() {

        // Populate the languages map
        languages = Stream.of(
                new Language("Java", LocalDate.of(1995, Month.MAY, 23), "9.0", "Object Oriented"),
                new Language("JavaScript", LocalDate.of(1995, Month.DECEMBER, 4), "9.0", "Prototype Based"))
                .collect(toMap(Language::getName, e -> e));
    }

    /**
     * Get one of the languages stored in the API with a delay
     * @param name - the name of the language
     * @return - the language object
     */
    public Language getLanguage(String name) {
        randomDelay();
        return languages.get(name);
    }

    /**
     * Create a random delay between 0-3 seconds
     */
    private void randomDelay() {
        double delay = random.nextDouble() * 3_000;
        System.out.println("The delay will be " + delay + " ms");
        try {
            Thread.sleep((long) delay);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
