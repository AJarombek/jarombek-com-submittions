// @author Andrew Jarombek
// @since 12/7/2017
// Demonstrate Native Getters and Setters in JavaScript

var person = {
    _first: 'Andy',
    _last: 'Jarombek',
    get first() {
        console.info('Accessing First Name');
        return this._first;
    },
    set first(str) {
        console.info('Setting First Name');
        if (typeof str === 'string' || str instanceof String) {
            var s = str.toLowerCase();
            this._first = s.charAt(0).toUpperCase() + s.slice(1);
        } else {
            throw new TypeError('First Name Must be a String');
        }
    },
    get last() {
        console.info('Accessing Last Name');
        return this._last;
    },
    set last(str) {
        console.info('Setting Last Name');
        if (typeof str === 'string' || str instanceof String) {
            var s = str.toLowerCase();
            this._last = s.charAt(0).toUpperCase() + s.slice(1);
        } else {
            throw new TypeError('Last Name Must be a String');
        }
    },
    get full() {
        console.info('Accessing Full Name');
        // Use ES6 template literals
        return `${this._first} ${this._last}`;
    }
};

// Native Getters allow us to call the getter just as we would any property
console.info(person.first);
console.info(person.last);
console.info(person.full); // Andy Jarombek

try {
	// Native Setters allow us to call the setter just as we would 
    // any property assignment
	person.first = 'JOSEPH';
	person.last = 25 // TypeError
} catch(e) {
	console.error(e);
}

console.info(person.full); // Joseph Jarombek