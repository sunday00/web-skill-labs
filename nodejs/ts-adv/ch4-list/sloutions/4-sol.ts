import { Equal, Expect } from "..";

/**
 * The ConvertTupleToArray type transforms a tuple type into an array of its elements.
 */
type ConvertTupleToArray<Tuple extends any[]> = Tuple[number][];

// Test cases to validate the ConvertTupleToArray type
type result1 = ConvertTupleToArray<[1, 2, 3]>;
// Expect result1 to be (1 | 2 | 3)[] since the elements of the tuple are converted to an array type
type check1 = Expect<Equal<result1, (1 | 2 | 3)[]>>;

type result2 = ConvertTupleToArray<[number, string]>;
// Expect result2 to be (number | string)[] since the elements of the tuple are converted to an array type
type check2 = Expect<Equal<result2, (number | string)[]>>;

type result3 = ConvertTupleToArray<[]>;
// Expect result3 to be never[] since the tuple is empty
type check3 = Expect<Equal<result3, never[]>>;

type result4 = ConvertTupleToArray<[1] | [2] | [3]>;
// Expect result4 to be (1 | 2 | 3)[] since the elements of the union of tuples are converted to an array type
type check4 = Expect<Equal<result4, (1 | 2 | 3)[]>>;
