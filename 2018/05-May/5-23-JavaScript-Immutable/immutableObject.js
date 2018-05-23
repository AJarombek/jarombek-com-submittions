/**
 * Testing out Immutable Programming for Objects in JavaScript
 * @author Andrew Jarombek
 * @since 5/21/2018
 */

/* Class with static helper methods for immutably manipulating objects */
class Objects {

    /* Add a new property to an object */
    static add(item, object) {
        return {
            ...object,
            ...item
        };
    };

    /* Replace a property in an object */
    static replace(newItem, object) {
        return Objects.add(newItem, object);
    };

    /* Remove a property from an object */
    static remove(item, object) {

        // Destructure the original object.  Unpack the property whose key matches
        // the string value of the 'item' parameter.  Then rename it to '_', which will
        // be ignored.  Copy the rest of the properties into the newObject.
        const {[item]: _, ...newObject} = object;
        console.info(`Removing ${JSON.stringify(_)}`);

        return newObject;
    };
}

/* Object representing a software developer */
const object = {
    name: "Andrew Jarombek",
    skills: ["Java", "JavaScript", "Etc"],
    job: {
        position: "Software Developer",
        company: "Gartner"
    }
};

const objectWithAge = Objects.add({age: 23}, object);

console.info(object);
console.info(objectWithAge);

const objectWithNewSkills = Objects.replace({skills: ["Java", "JavaScript"]}, object);

console.info(object);
console.info(objectWithNewSkills);

const objectNoSkills = Objects.remove('job', object);

console.info(object);
console.info(objectNoSkills);