"use strict";
/**
 * Helper functions for testing object value equality.
 * @author Andrew Jarombek
 * @since 8/10/2019
 */
Object.defineProperty(exports, "__esModule", { value: true });
var EqualityHelper = /** @class */ (function () {
    function EqualityHelper() {
    }
    /**
     * Test for value equality on two objects.
     * NOTE: nested objects are tested for reference equality.
     * @param obj1 the first object to compare for equality.
     * @param obj2 the second object to compare for equality.
     * @return {boolean} {@code true} if the objects (un-nested) property values are equal,
     * {@code false} otherwise.
     */
    EqualityHelper.equals = function (obj1, obj2) {
        var aProps = Object.getOwnPropertyNames(obj1);
        var bProps = Object.getOwnPropertyNames(obj2);
        if (aProps.length !== bProps.length) {
            return false;
        }
        for (var i = 0; i < aProps.length; i++) {
            var propName = aProps[i];
            if (obj1[propName] !== obj2[propName]) {
                return false;
            }
        }
        return true;
    };
    return EqualityHelper;
}());
exports.EqualityHelper = EqualityHelper;
