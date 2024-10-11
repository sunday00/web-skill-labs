import { Equal, Expect } from "..";

/**
 * The AddToEnd type adds an element to the end of a tuple.
 */
type AddToEnd<Tuple extends any[], Element> = [...Tuple, Element];

// Test cases to validate the AddToEnd type
type result1 = AddToEnd<[1, 2, 3], 4>;
// Expect result1 to be [1, 2, 3, 4] since 4 is added to the end
type test1 = Expect<Equal<result1, [1, 2, 3, 4]>>;

type result2 = AddToEnd<[], 1>;
// Expect result2 to be [1] since 1 is added to the empty tuple
type test2 = Expect<Equal<result2, [1]>>;
