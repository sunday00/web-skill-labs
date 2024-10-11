import { Equal, Expect } from "..";

/**
 * Type utility to perform logical AND operation on two boolean types.
 * It evaluates to `true` only if both `A` and `B` are `true`.
 *
 ** Hint: you can check several values at once by wrapping
 **       them in a tuple type (pattern matching).
 */
type LogicalAND<A, B> = [A, B] extends [true, true] ? true : false;

// Test cases to validate the LogicalAND type utility

type result1 = LogicalAND<true, true>;
type testCase1 = Expect<Equal<result1, true>>;

type result2 = LogicalAND<false, false>;
type testCase2 = Expect<Equal<result2, false>>;

type result3 = LogicalAND<true, false>;
type testCase3 = Expect<Equal<result3, false>>;

type result4 = LogicalAND<false, true>;
type testCase4 = Expect<Equal<result4, false>>;
