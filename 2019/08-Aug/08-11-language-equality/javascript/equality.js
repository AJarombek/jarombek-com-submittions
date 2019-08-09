/**
 * Demonstrate how equality between types works in JavaScript
 * @author Andrew Jarombek
 * @since 8/8/2019
 */

const assert = (assertion) => {
    console.assert(assertion, `Assertion failed!`);
};

/**
 * primitive types == and ===
 */

// JavaScript has implicit and explicit type coercion. The == operator allows for implicit type
// coercion when comparing values.  The === operator does not.  When working with primitive types,
// both == and === test value equality.

// If a number is compared to a string using ==, the string is implicitly converted to a number
// before checking equality.
assert(2 == "2");
assert("2" == 2);

// This is the same as:
assert(2 === Number("2"));

// If a number is compared to a string using !==, no type coercion occurs.
assert(2 !== "2");
assert("2" !== 2);

// Comparing two of the same primitive built-in types (null, undefined, boolean, number, string,
// symbol) results in true if their values are equal (as expected).
assert("hello" == "hello");
assert("hello" === "hello");

// Because of unusual behavior, you should never compare a type to a boolean using ==.

// This first converts 'true' to its number equivalent, which is 1.  Then it converts "0" to a
// number.  Then it compares 1 === 0, which is false.
assert(true != "0");

// This first converts 'false' to its number equivalent, which is 0.  Then it converts "0" to a
// number.  Then it compares 0 === 0, which is true.
assert(false == "0");

// 'true' converts to 1, "1" converts to 1.  The expression 1 === 1 is true.
assert(true == "1");

// 'true' converts to 1, "2" converts to 2.  The expression 1 === 2 is false.
assert(true != "2");

// Since null and undefined are both falsey, comparing them with == returns true.
assert(undefined == null);
assert(null == undefined);

// But with === they return false.
assert(undefined !== null);
assert(null !== undefined);

// However, null and undefined are not equal to any other types, even if their values are falsey
assert(null != "");
assert(null !== "");
assert(null != 0);
assert(null !== 0);
assert(null != false);
assert(null !== false);

// When an object type is compared to a primitive type with ==, the object is first coerced
// into a primitive value.

// An array is a type of object.
assert([10] == 10);
assert([10] == "10");
assert([10] !== "10");

// A function is also a type of object.
assert(() => "hello" == "hello");
assert(() => "100" == "100");
assert(() => "100" == 100);
assert(() => "100" !== 100);

// Objects containing a property with a certain value aren't coerced into that value.
assert({value: 1} != 1);

// However, objects created by passing a primitive value into an object constructor
// are coerced to that value.
assert(new Object(1) == 1);
assert(new Object(1) !== 1);

// You can monkey-patch prototype functions to create strange equality behavior.
const twentyTwo = new Number(22);

assert(23 != 24);
assert(twentyTwo != 23); // 22 != 23 as expected

Number.prototype.valueOf = () => {
    return 23;
};

assert(23 != 24);
assert(twentyTwo == 23); // 22 == 23 unexpectedly due to valueOf() monkey patch.

/**
 * object types == and ===
 */

// For object types, reference equality is used.  == and === behave identically when
// comparing two objects.
assert({} != {});
assert({} !== {});

// Remember that arrays and functions are also objects in JavaScript.
assert([] != []);
assert([] !== []);
assert((() => {}) != (() => {}));
assert((() => {}) !== (() => {}));

const emptyObj1 = {};

// emptyObj2 references the same object in memory.
const emptyObj2 = emptyObj1;

// emptyObj3 creates a new object with emptyObj1 as its prototype.
const emptyObj3 = Object.create(emptyObj1);

// emptyObj4 uses emptyObj1 as a base object and copies the empty object into the base object.
const emptyObj4 = Object.assign(emptyObj1, {});

assert(emptyObj1 == emptyObj2);
assert(emptyObj1 === emptyObj2);

assert(emptyObj1 != emptyObj3);
assert(emptyObj1 !== emptyObj3);

assert(emptyObj1 == emptyObj4);
assert(emptyObj1 === emptyObj4);

/**
 * Test for value equality on two objects.  NOTE: nested objects are tested for reference equality.
 * @param a the first object to compare for equality.
 * @param b the second object to compare for equality.
 * @return {boolean} {@code true} if the objects (un-nested) property values are equal,
 * {@code false} otherwise.
 */
const equals = (a, b) => {
    const aProps = Object.getOwnPropertyNames(a);
    const bProps = Object.getOwnPropertyNames(b);

    if (aProps.length !== bProps.length) {
        return false;
    }

    for (let i = 0; i < aProps.length; i++) {
        const propName = aProps[i];
        if (a[propName] !== b[propName]) {
            return false;
        }
    }

    return true;
};

// Prove that equals() tests for value equality on object properties.
assert(equals({}, {}));
assert(equals({first: 'andy', last: 'jarombek'}, {last: 'jarombek', first: 'andy'}));

/**
 * fun edge cases for ==
 */