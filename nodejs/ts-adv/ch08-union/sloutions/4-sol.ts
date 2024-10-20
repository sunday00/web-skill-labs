import { Equal, Expect } from "..";

type FalsyValues = false | null | 0 | undefined | "";

// Function to filter out falsy values from an array
declare function filterFalsy<Item>(array: Item[]): Exclude<Item, FalsyValues>[];
//                 👆
// We can use `Exclude` to remove falsy values
// From our union type.

let result1 = filterFalsy([1, 2, null, 3, undefined, 4]);
type test1 = Expect<Equal<typeof result1, number[]>>;

let result2 = filterFalsy([undefined, 0 as const, "a", "", "b", "c", "d"]);
type test2 = Expect<Equal<typeof result2, string[]>>;

let result3 = filterFalsy([
  ...([1, undefined, true, "", "hello", null] as const),
]);
type test3 = Expect<Equal<typeof result3, (1 | true | "hello")[]>>;
