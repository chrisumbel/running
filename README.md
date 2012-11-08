# running

Franklin's fork.

## Tests

Write the tests first: test-driven development, always!

Apart from some typical HUnit-style tests, there is an illustration of how to use QuickCheck to test properties.

## Error checking and types

There should really be error checking beyond just a pattern match failure.

Introduction of types would be appropriate for production code:

- a type representing total number of seconds
- a type representing a clock time
- a type each for clock hours, minutes, and seconds
