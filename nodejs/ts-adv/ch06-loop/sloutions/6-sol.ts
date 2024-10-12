import { Equal, Expect } from "..";

/**
 * Create a `MapWithIndex` type function
 * that takes a tuple and maps it to a tuple
 * of [value, index] pairs.
 *
 */

type MapWithIndex<Tuple extends any[], Output extends any[] = []> =
  // Step 1: Split the tuple
  Tuple extends [infer First, ...infer Rest]
    ? // Step 2: Recursively call `MapWithIndex` on `Rest`, and add a new entry to `Output`.
      // We use the previous length of `Output` to get the current index.
      MapWithIndex<Rest, [...Output, [First, Output["length"]]]>
    : // Step 3: If the tuple is empty, return the output.
      Output;

// Example usage and tests
type example1 = MapWithIndex<["x"]>;
type test1 = Expect<Equal<example1, [["x", 0]]>>;

type example2 = MapWithIndex<["x", "y"]>;
type test2 = Expect<Equal<example2, [["x", 0], ["y", 1]]>>;

type example3 = MapWithIndex<["x", "y", "z"]>;
type test3 = Expect<Equal<example3, [["x", 0], ["y", 1], ["z", 2]]>>;
