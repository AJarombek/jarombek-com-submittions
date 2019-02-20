import groovy.transform.Immutable

/**
 * Basic Groovy class representing a person with an explicitly declared constructor.  The class is also
 * immutable, so once it is constructed all its fields are effectively final
 * @author Andrew Jarombek
 * @since 7/4/2018
 */

@Immutable
class ImmutablePerson {
    String first
    String last
}