import groovy.transform.Immutable

/**
 * Groovy class representing a Cat.  It has an implicit constructor, which will be passed
 * named parameters.  It also is immutable, so it can not be altered after construction
 * @author Andrew Jarombek
 * @since 7/4/2018
 */

@Immutable
class ImmutableCat {
    String name
    String breed
    ImmutablePerson owner
}