import { Equal, Expect } from "..";

/**
 * The CombineTuples type merges two tuples into one.
 */
type CombineTuples<Tuple1 extends any[], Tuple2 extends any[]> = [
  ...Tuple1,
  ...Tuple2
];

// Test cases to validate the CombineTuples type
type result1 = CombineTuples<[1, 2, 3], [4, 5]>;
// Expect result1 to be [1, 2, 3, 4, 5] since the elements of both tuples are combined
type test1 = Expect<Equal<result1, [1, 2, 3, 4, 5]>>;

type result2 = CombineTuples<[1, 2, 3], []>;
// Expect result2 to be [1, 2, 3] since the second tuple is empty
type test2 = Expect<Equal<result2, [1, 2, 3]>>;
