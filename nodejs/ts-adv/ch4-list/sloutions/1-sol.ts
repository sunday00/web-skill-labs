import { Equal, Expect } from "..";

/**
 * The GetFirstElement type extracts the first element type from a tuple.
 * If the tuple is empty, it returns undefined.
 */
type GetFirstElement<Tuple extends any[]> = Tuple[0];

// Test cases to validate the GetFirstElement type
type result1 = GetFirstElement<[]>;
// Expect result1 to be undefined since the tuple is empty
type test1 = Expect<Equal<result1, undefined>>;

type result2 = GetFirstElement<[string]>;
// Expect result2 to be string since the first element is of type string
type test2 = Expect<Equal<result2, string>>;

type result3 = GetFirstElement<[2, 3, 4]>;
// Expect result3 to be number 2 since it's the first element of the tuple
type test3 = Expect<Equal<result3, 2>>;

type result4 = GetFirstElement<["a", "b", "c"]>;
// Expect result4 to be string "a" since it's the first element of the tuple
type test4 = Expect<Equal<result4, "a">>;
