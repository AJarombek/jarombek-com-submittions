/**
 * Testing out Immutable Programming for Arrays in JavaScript
 * @author Andrew Jarombek
 * @since 5/21/2018
 */

/* Class with static helper methods for immutably manipulating arrays */
class Arrays {

    /* Add a new element to an array */
    static add(item, array) {
        return [
            ...array,
            item
        ];
    };

    /* Replace an element in an array */
    static replace(item, newItem, array) {
        return array.map((i) => {
            return (i === item) ? newItem : i;
        });
    };

    /* Remove an element from an array */
    static remove(item, array) {
        return array.filter((i) => i !== item);
    };
}

const array = [1, 2, 4, 8, 16];

const thirtyTwoArray = Arrays.add(32, array);

console.info(array);
console.info(thirtyTwoArray);

const zeroMiddleArray = Arrays.replace(4, 0, array);

console.info(array);
console.info(zeroMiddleArray);

const noEightArray = Arrays.remove(8, array);

console.info(array);
console.info(noEightArray);