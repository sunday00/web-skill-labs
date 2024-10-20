import { Equal, Expect } from "..";

type CheckIsNever<T> = [T] extends [never] ? true : false;

// Test cases
type testCase1 = CheckIsNever<never>;
type test1 = Expect<Equal<testCase1, true>>;

type testCase2 = CheckIsNever<"not never">;
type test2 = Expect<Equal<testCase2, false>>;

type testCase3 = CheckIsNever<1 | 2 | never>;
type test3 = Expect<Equal<testCase3, false>>;
