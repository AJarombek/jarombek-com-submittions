package clone;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Mutable class that represents a programming language
 * @author Andrew Jarombek
 * @since 5/14/2018
 */
public class Language implements Cloneable {

    private String name;
    private LocalDate inception;
    private List<Paradigm> paradigms;

    enum Paradigm {
        Object_Oriented, Imperative, Functional, Declarative
    }

    public Language(String name, LocalDate inception, List<Paradigm> paradigms) {
        this.name = name;
        this.inception = inception;
        this.paradigms = new ArrayList<>(paradigms);
    }

    /* Copy Constructor */
    public Language(Language language) {
        this.name = language.name;
        this.inception = language.inception;
        this.paradigms = new ArrayList<>(language.paradigms);
    }

    /* Copy Static Factory Method */
    public static Language create(Language language) {
        return new Language(language);
    }

    @Override
    protected Language clone() {
        try {
            Language language = (Language) super.clone();
            language.paradigms = new ArrayList<>(paradigms);
            return language;
        } catch(CloneNotSupportedException ex) {
            throw new AssertionError();
        }
    }

    @Override
    public String toString() {
        return "[" + name + ", " + inception.toString() + ", " + paradigms.toString() + "]";
    }

    /* Getters and Setters */

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public LocalDate getInception() {
        return inception;
    }

    public void setInception(LocalDate inception) {
        this.inception = inception;
    }

    public List<Paradigm> getParadigms() {
        return paradigms;
    }

    public void setParadigms(List<Paradigm> paradigms) {
        this.paradigms = paradigms;
    }

    // Add a paradigm to the list if a non null value is passed in
    public void addParadigm(Paradigm paradigm) {
        Optional.ofNullable(paradigm).ifPresent(p -> this.paradigms.add(p));
    }
}