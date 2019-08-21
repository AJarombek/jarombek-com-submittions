/**
 * Helper functions for testing object value equality.
 * @author Andrew Jarombek
 * @since 8/10/2019
 */

export class EqualityHelper {

    /**
     * Test for value equality on two objects.
     * NOTE: nested objects are tested for reference equality.
     * @param obj1 the first object to compare for equality.
     * @param obj2 the second object to compare for equality.
     * @return {boolean} {@code true} if the objects (un-nested) property values are equal,
     * {@code false} otherwise.
     */
    public static equals(obj1, obj2): boolean {
        const aProps: Array<string> = Object.getOwnPropertyNames(obj1);
        const bProps: Array<string> = Object.getOwnPropertyNames(obj2);

        if (aProps.length !== bProps.length) {
            return false;
        }

        for (let i: number = 0; i < aProps.length; i++) {
            const propName: string = aProps[i];
            if (obj1[propName] !== obj2[propName]) {
                return false;
            }
        }

        return true;
    }
}