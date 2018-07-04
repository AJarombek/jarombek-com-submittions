#!/usr/bin/env groovy

/**
 * Invoking Groovy classes
 * @author Andrew Jarombek
 * @since 7/3/2018
 */

def andy = new Person('Andrew', 'Jarombek')
println andy

Person tom = ['Thomas', 'Cauliflower']
println tom

def lily = new Cat(name: 'Lily', breed: 'Russian Blue')
println lily

lily.sayHello()

def joe = new Cat(name: 'Joe')
println joe