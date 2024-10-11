import { Equal, Expect } from "..";

/**
 * Utility type to get the length of a tuple plus one.
 */
type ExtendedLength<Tuple extends any[]> = [...Tuple, any]["length"];

// Example usage and tests

type example1 = ExtendedLength<[]>;
// Test case 1: The length of an empty tuple plus one should be 1
type test1 = Expect<Equal<example1, 1>>;

type example2 = ExtendedLength<[any]>;
// Test case 2: The length of a tuple with one element plus one should be 2
type test2 = Expect<Equal<example2, 2>>;

type example3 = ExtendedLength<[any, any]>;
// Test case 3: The length of a tuple with two elements plus one should be 3
type test3 = Expect<Equal<example3, 3>>;

type example4 = ExtendedLength<[any, any, any]>;
// Test case 4: The length of a tuple with three elements plus one should be 4
type test4 = Expect<Equal<example4, 4>>;
