import { Equal, Expect } from "..";

declare function combineArrays<
  // Infer `ArrList` as a tuple of arrays:
  ArrList extends [any[], ...any[][]]
>(...arrays: ArrList): MergeLists<ArrList>[];

// This is a map loop!
type MergeLists<List> =
  // We use a single conditional type to split the
  // list and unwrap the first array:
  List extends [(infer Value)[], ...infer Rest]
    ? // Recurse on `Rest`:
      [Value, ...MergeLists<Rest>]
    : // If the list is empty, return an empty array:
      [];

// Example usage and tests
const example1 = combineArrays([1, 2], [true, false]);
// => [[1, true], [2, false]]
type test1 = Expect<Equal<typeof example1, [number, boolean][]>>;

const example2 = combineArrays([1, 2], [true, false], ["a", "b"]);
// => [[1, true, 'a'], [2, false, 'b']]
type test2 = Expect<Equal<typeof example2, [number, boolean, string][]>>;

const example3 = combineArrays([1, 2, null], [true, false, undefined]);
// => [[1, true], [2, false], [null, undefined]]
type test3 = Expect<
  Equal<typeof example3, [number | null, boolean | undefined][]>
>;
