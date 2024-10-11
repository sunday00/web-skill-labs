import { Equal, Expect } from "..";

/**
 * Utility type to get the length of a tuple.
 */
type TupleLength<T extends any[]> = T["length"];

// Example usage and tests

type result1 = TupleLength<[]>;
// Test case 1: The length of an empty tuple should be 0
type test1 = Expect<Equal<result1, 0>>;

type result2 = TupleLength<[any]>;
// Test case 2: The length of a tuple with one element should be 1
type test2 = Expect<Equal<result2, 1>>;

type result3 = TupleLength<[any, any]>;
// Test case 3: The length of a tuple with two elements should be 2
type test3 = Expect<Equal<result3, 2>>;

type result4 = TupleLength<[any, any, any]>;
// Test case 4: The length of a tuple with three elements should be 3
type test4 = Expect<Equal<result4, 3>>;
