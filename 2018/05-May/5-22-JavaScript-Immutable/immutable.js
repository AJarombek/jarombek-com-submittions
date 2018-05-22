/**
 * Testing out Immutable Programming in JavaScript
 * @author Andrew Jarombek
 * @since 5/21/2018
 */

/* Const can't be rebound to a new object */
const name = 'Andy';

/* Throws TypeError: Assignment to constant variable. */
// name = 'Joe';

/* Let can be bound to a new object */
let othername = 'Andy';
othername = 'Joe';

console.info(othername); // Joe

/*
Although they cant bind to a new value, the value a const variable
is currently bound to can change.  This means const is NOT immutable
*/
const names = ["Andrew", "Jarombek"];
names[0] = "Joe";

console.info(`Names After Modification: ${names}`);

/* Throws TypeError: Assignment to constant variable. */
// names = ["Joe", "Jarombek"];

// Freezing the names array makes it read only
Object.freeze(names);

names[0] = "Tom";
console.info(`Names After Frozen: ${names}`);

/* Sealing objects */

const cat = {
    breed: "Russian Blue",
    name: "Lily"
};

Object.seal(cat);

/* Sealed objects allow for properties to be modified... */
cat.name = "Dotty";

/* ...But they do not allow for new properties to be added */
cat.owner = "Andy";

console.info(`Sealed Cat After Modification: ${JSON.stringify(cat)}`);

/* Freezing complex objects */

const person = {
    name: "Andy",
    hometown: {
        city: "Riverside",
        state: "Connecticut",
        country: "USA"
    }
};

Object.freeze(person);

const isFrozen = Object.isFrozen(person);
console.info(`Is Person Frozen: ${isFrozen}`);

// Object is frozen, this mutation has no effect
person.name = "Joe";

// Freezing is shallow, so nested objects can be mutated!
person.hometown.country = "Canada";

console.info(person);

/* Immutable objects ES6 plus */

// The spread operator copies all the properties from one object to another
// With it you can maintain mutability
const changedNamePerson = {
    ...person,
    name: "Joe"
};

console.info(`The Original Person: ${JSON.stringify(person)}`);
console.info(`The New Person: ${JSON.stringify(changedNamePerson)}`);

/*
Object.assign() creates a copy of an object with a change
Param 1: The target of the copy.
Param 2: The original object to be copied.
Param 3: Changes that will be made to the newly created object.
 */
const anotherNewPerson = Object.assign({}, changedNamePerson, {name: "Tom"});

console.info(`The Original Person: ${JSON.stringify(changedNamePerson)}`);
console.info(`The New(er) Person: ${JSON.stringify(anotherNewPerson)}`);