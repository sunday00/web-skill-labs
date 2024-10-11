import { Equal, Expect } from "..";

/**
 * Type utility to remove the first element from a tuple.
 * If the tuple is empty, it returns an empty tuple.
 */
type RemoveFirstElement<Tuple extends any[]> = Tuple extends [
  any,
  ...infer RemainingElements
]
  ? RemainingElements
  : [];

// Test cases to validate the RemoveFirstElement type utility

type result1 = RemoveFirstElement<[1, 2, 3]>;
type testCase1 = Expect<Equal<result1, [2, 3]>>;

type result2 = RemoveFirstElement<[1]>;
type testCase2 = Expect<Equal<result2, []>>;

type result3 = RemoveFirstElement<[]>;
type testCase3 = Expect<Equal<result3, []>>;
