import { Equal, Expect } from "..";

/**
 * Create a `TakeFirst` type-level function
 * that takes a tuple and a number `N`,
 * and returns the first `N` elements of this tuple.
 *
 * Hint: you will need to use T["length"]
 * to read the length of a tuple `T`.
 */

type TakeFirst<Tuple extends any[], N, Output extends any[] = []> =
  // 1. If the output has the desired length, return it:
  Output["length"] extends N
    ? Output
    : // 2. If the input tuple isn't empty,
    //    add its first value to the output
    //    and recurse on the rest:
    Tuple extends [infer First, ...infer Rest]
    ? TakeFirst<Rest, N, [...Output, First]>
    : // 3. If the input tuple is empty, return the output:
      Output;

// Example usage and tests
type example1 = TakeFirst<[1, 2, 3], 2>;
type test1 = Expect<Equal<example1, [1, 2]>>;

type example2 = TakeFirst<[1, 2, 3], 1>;
type test2 = Expect<Equal<example2, [1]>>;

type example3 = TakeFirst<[1, 2, 3], 0>;
type test3 = Expect<Equal<example3, []>>;

type example4 = TakeFirst<[1, 2], 5>;
type test4 = Expect<Equal<example4, [1, 2]>>;
