import { Equal, Expect } from "..";

type CompareTypes<A, B> =
  // Wrapping parameters in tuples to prevent distribution:
  [A] extends [B]
    ? // Check if A and B are assignable in both directions
      // This indicates they are the same type OR one of
      // them is `any`.
      [B] extends [A]
      ? true
      : false
    : false;

// Test cases
type testCase1 = CompareTypes<"a", "a">;
type test1 = Expect<Equal<testCase1, true>>;

type testCase2 = CompareTypes<"a", "b">;
type test2 = Expect<Equal<testCase2, false>>;

type testCase3 = CompareTypes<1 | 2, 1 | 2>;
type test3 = Expect<Equal<testCase3, true>>;

type testCase4 = CompareTypes<1 | 2, 2 | 3>;
type test4 = Expect<Equal<testCase4, false>>;

type testCase5 = CompareTypes<1 | 2, never>;
type test5 = Expect<Equal<testCase5, false>>;

type testCase6 = CompareTypes<never, 1 | 2>;
type test6 = Expect<Equal<testCase6, false>>;
