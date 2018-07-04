import groovy.transform.ToString

/**
 * Groovy class representing a Cat.  It has an implicit constructor, which will be passed
 * named parameters
 * @author Andrew Jarombek
 * @since 7/3/2018
 */

@ToString(includeNames = true)
class Cat {
    String name
    String breed

    def sayHello() {
        println "$name says Meow!"
    }
}