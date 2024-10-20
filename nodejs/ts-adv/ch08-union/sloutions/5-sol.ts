import { Equal, Expect } from "..";

declare function divideArray<Item, Filtered extends Item>(
  //                                         👆
  //                     `Filtered` has to be assignable to `Item`
  //                         to be usable in a type guard.
  array: Item[],
  condition: (value: Item) => value is Filtered
): [Filtered[], Exclude<Item, Filtered>[]];
//      👆                        👆
// We do not need To use        We filter the `Item`
// `Extract` here since         type to only include members
// `Filtered` is already        that aren't assignable to
// what we want.                `Filtered`.

const example1 = divideArray(
  [1, 2, "N/A", 7, "oops"],
  (x): x is number => typeof x === "number"
);
type test1 = Expect<Equal<typeof example1, [number[], string[]]>>;

const example2 = divideArray(
  [true, false, 1, true, 0],
  (x): x is boolean => typeof x === "boolean"
);
type test2 = Expect<Equal<typeof example2, [boolean[], number[]]>>;

const example3 = divideArray(
  ["value", "onChange", "onSubmit", "valid", "focused"],
  (x): x is `on${string}` => x.startsWith("on")
);
type test3 = Expect<Equal<typeof example3, [`on${string}`[], string[]]>>;
