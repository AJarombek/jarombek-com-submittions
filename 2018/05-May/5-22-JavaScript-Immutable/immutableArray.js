/**
 * Testing out Immutable Programming for Arrays in JavaScript
 * @author Andrew Jarombek
 * @since 5/21/2018
 */

class Arrays {

    static add(item, array) {
        return [
            ...array,
            item
        ];
    };

    static edit(item, newItem, array) {
        return array.map((i) => {
            return (i === item) ? newItem : i;
        });
    };

    static remove(item, array) {
        return array.filter((i) => i !== item);
    };
}

const array = [1, 2, 4, 8, 16];

const thirtyTwoArray = Arrays.add(32, array);

console.info(array);
console.info(thirtyTwoArray);

const zeroMiddleArray = Arrays.edit(4, 0, array);

console.info(array);
console.info(zeroMiddleArray);

const noEightArray = Arrays.remove(8, array);

console.info(array);
console.info(noEightArray);