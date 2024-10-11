import { Equal, Expect } from "..";

type UnwrapNestedArrays<ArrayType extends any[]> =
  ArrayType extends (infer Element)[][] ? Element[] : ArrayType;

function mergeNestedArrays<ArrayType extends any[]>(
  nestedArray: ArrayType
): UnwrapNestedArrays<ArrayType> {
  return nestedArray.reduce(
    (accumulator, item) => [
      ...accumulator,
      ...(Array.isArray(item) ? item : [item]),
    ],
    []
  );
}

// No nested arrays
let result1 = mergeNestedArrays(["a", "b", "c", "d"]);
type testCase1 = Expect<Equal<typeof result1, string[]>>;

// One level of nested arrays
let result2 = mergeNestedArrays([
  [1, 2],
  [3, 4],
]);
type testCase2 = Expect<Equal<typeof result2, number[]>>;

// Two levels of nested arrays
let result3 = mergeNestedArrays([[["a"], ["b", "c"]], [["d"]]]);
type testCase3 = Expect<Equal<typeof result3, string[][]>>;

// Three levels of nested arrays
let result4 = mergeNestedArrays([[[[true]], [[false, true]]], [[[true]]]]);
type testCase4 = Expect<Equal<typeof result4, boolean[][][]>>;
