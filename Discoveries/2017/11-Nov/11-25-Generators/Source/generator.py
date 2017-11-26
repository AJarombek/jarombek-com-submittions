# Author: Andrew Jarombek
# Date: 11/26/2017
# Fibonacci list using a generator

def fib():
    first = 0
    last = 1
    
    yield first
    yield last
    
    while True:
        # Construct and deconstruct a tuple
        first, last = last, first + last
        yield last

fibonacci = fib()
for x in range(50):
    print(next(fibonacci))