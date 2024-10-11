import { Equal, Expect } from "..";

/**
 * Type utility to extract the last element of a tuple.
 * If the tuple is empty, it returns `never`.
 */
type GetLastElement<Tuple extends any[]> = Tuple extends [
  ...any[],
  infer LastElement
]
  ? //      👆
    // This will match all elements in the tuple, except the last one.
    LastElement
  : // 👆
    // `infer` defines a new type variable which we return here.
    never;

// Test cases to validate the GetLastElement type utility

type result1 = GetLastElement<[1, 2, 3]>;
type testCase1 = Expect<Equal<result1, 3>>;

type result2 = GetLastElement<[1]>;
type testCase2 = Expect<Equal<result2, 1>>;

type result3 = GetLastElement<[]>;
type testCase3 = Expect<Equal<result3, never>>;
