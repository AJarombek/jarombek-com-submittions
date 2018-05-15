package clone;

import java.time.LocalDate;
import java.time.Month;
import java.util.Arrays;
import java.util.List;

/**
 * Test out different cloning methods in Java
 * @author Andrew Jarombek
 * @since 5/14/2018
 */
public class Main {

    public static void main(String... args) {

        Language language = new Language("Java",
                LocalDate.of(1995, Month.MAY, 23),
                List.of(Language.Paradigm.Object_Oriented, Language.Paradigm.Imperative));

        Language language2 = language.clone();

        Language language3 = new Language(language);
        Language language4 = Language.create(language);

        System.out.println(language);
        System.out.println(language2);
        System.out.println(language3);
        System.out.println(language4);

        // Name is immutable and Inception is a final singleton, so changing them
        // has no impact on the copied language
        language.setName("Prolog");
        language.setInception(LocalDate.EPOCH);

        // This mutation will not effect any of the copies
        language.addParadigm(Language.Paradigm.Declarative);

        System.out.println(language);
        System.out.println(language2);
        System.out.println(language3);
        System.out.println(language4);

        // Cloning is commonly used with arrays
        String[] strings = new String[2];
        strings[0] = "Whats";
        strings[1] = "Up";

        String[] stringsCopy = strings.clone();

        stringsCopy[1] = "Going On";

        System.out.println(Arrays.toString(strings));
        System.out.println(Arrays.toString(stringsCopy));

        // Note that the contents of the arrays are not cloned, so if they are mutable
        // changing them will effect both arrays
        Language[] languages = new Language[2];

        languages[0] = new Language("Java",
                LocalDate.of(1995, Month.MAY, 23),
                List.of(Language.Paradigm.Object_Oriented, Language.Paradigm.Imperative));
        languages[1] = new Language("JavaScript",
                LocalDate.of(1995, Month.DECEMBER, 4),
                List.of(Language.Paradigm.Object_Oriented, Language.Paradigm.Imperative,
                        Language.Paradigm.Functional));

        // Clone array of mutable languages
        Language[] languagesCopy = languages.clone();

        System.out.println(Arrays.toString(languages));
        System.out.println(Arrays.toString(languagesCopy));

        // Change the date Java was created to the epoch
        languagesCopy[0].setInception(LocalDate.EPOCH);

        System.out.println(Arrays.toString(languages));
        System.out.println(Arrays.toString(languagesCopy));
    }
}
