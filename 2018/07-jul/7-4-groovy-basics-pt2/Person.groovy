import groovy.transform.ToString

/**
 * Basic Groovy class with an explicitly declared constructor
 * @author Andrew Jarombek
 * @since 7/3/2018
 */

@ToString
class Person {
    String first
    String last

    Person(first, last) {
        this.first = first
        this.last = last
    }
}