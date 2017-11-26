// @author Andrew Jarombek
// @since 8/16/2017
// Tying out Generators in JavaScript

// A generator is declared with a star.
// Generators can pause themselves mid-execution.  They act like an iterator.
function *push() {
	var x, y;

	// Yield statements pause the execution of the generator.
	var array = [x = yield, y = yield];
    console.info(array);
    console.info(x);
    console.info(y);
}

var pushIt = push();

pushIt.next('start');
pushIt.next(5);
pushIt.next(10);

// Fibonacci number problem solved using a generator
function *fib() {
	var first = 0;
    var last = 1;
  
    yield first;
    yield last;
  
    while(true) {
        // ES6 Desconstructing shortens the traditional variable swap
        [first, last] = [last, first + last];
        
        yield last;
    }
}

var fibonacci = fib();

for(let i = 0; i < 50; i++) {
	let result = fibonacci.next();
    console.info(result.value);
}