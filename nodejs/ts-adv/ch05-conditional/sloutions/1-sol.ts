import { Equal, Expect } from "..";

/**
 * Type utility to check if a given type is an array.
 * It uses conditional types to determine if `T` extends an array type.
 */
type CheckIfArray<T> = T extends unknown[] ? true : false;

// Example usage and tests

type example1 = CheckIfArray<number[]>;
// Test case 1: `number[]` should be identified as an array type
type test1 = Expect<Equal<example1, true>>;

type example2 = CheckIfArray<["a", "b", "c"]>;
// Test case 2: `["a", "b", "c"]` should be identified as an array type
type test2 = Expect<Equal<example2, true>>;

type example3 = CheckIfArray<"Not an array">;
// Test case 3: `"Not an array"` should not be identified as an array type
type test3 = Expect<Equal<example3, false>>;

type example4 = CheckIfArray<string | null | undefined>;
// Test case 4: `string | null | undefined` should not be identified as an array type
type test4 = Expect<Equal<example4, false>>;
