#!/usr/bin/env groovy

/**
 * Experiment with the meta object protocol.
 * @author Andrew Jarombek
 * @since 9/8/2018
 */

class LanguageUse {

    static def lastMonth = [
        "javascript": 1656,
        "html": 658,
        "groovy": 484,
        "sass": 387,
        "hcl": 366,
        "haskell": 138,
        "bash": 90,
        "java": 30,
        "swift": 30,
        "velocity": 29,
        "json": 8
    ]

    /**
     * If the method call does not match any on the LanguageUse class, this method is invoked.  In that case,
     * check to see if the method name (minus the 'get' prefix) matches a key in the {@code lastMonth} map.  If so,
     * return the value corresponding to the key.
     * @param name - the name of the invoked method
     * @param args - arguments passed to the invoked method
     * @return A value in the {@code lastMonth} map matching the method name (minus the 'get' prefix)
     */
    def methodMissing(String name, Object args) {
        def language = name - "get"
        return lastMonth[language.toLowerCase()]
    }

    /**
     * If a property accessed on an instance of {@code LanguageUse} does not exist, this method is invoked.
     * In that case, look for a key in the {@code lastMonth} map that matches the property name.
     * @param name - the name of the property accessed
     * @return A value in the {@code lastMonth} map matching the property name
     */
    def propertyMissing(String name) {
        return lastMonth[name]
    }
}

def languageUse = new LanguageUse()

// Call methods that go through the MOP and methodMissing()
assert languageUse.getJavaScript() == 1656
assert languageUse.getGroovy() == 484
assert languageUse.getJava() == 30

// Access properties that go through the MOP and propertyMissing()
assert languageUse.javascript == 1656
assert languageUse.groovy == 484
assert languageUse.java == 30