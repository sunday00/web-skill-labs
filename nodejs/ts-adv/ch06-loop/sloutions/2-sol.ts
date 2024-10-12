import { Equal, Expect } from "..";

/**
 * Type the `all` function to take a list of promises and
 * to turn them into a single promise containing a list of values.
 */

declare function waitForAll<
  // Infer `PromiseArray` as a tuple of promises:
  PromiseArray extends [Promise<any>, ...Promise<any>[]]
>(promises: PromiseArray): AwaitAll<PromiseArray>;

// Unwrap all promises and wrap the resulting tuple type in a `Promise`:
type AwaitAll<PromiseArray> = Promise<UnwrapAll<PromiseArray>>;

// This is a "map" loop!
type UnwrapAll<PromiseArray> =
  // 1. Split the list, and infer the promise's `value`:
  PromiseArray extends [Promise<infer Value>, ...infer Rest]
    ? // 2. Add the value to the array, recurse on `Rest`:
      [Value, ...UnwrapAll<Rest>]
    : // 3. If the list is empty, return an empty list:
      [];

// Two promises
const result1 = waitForAll([
  Promise.resolve(20),
  Promise.resolve("Hello" as const),
]);
type expected1 = Promise<[number, "Hello"]>;
type test1 = Expect<Equal<typeof result1, expected1>>;

// Three promises
const result2 = waitForAll([
  Promise.resolve(true),
  Promise.resolve("!"),
  Promise.resolve({}),
]);
type expected2 = Promise<[boolean, string, {}]>;
type test2 = Expect<Equal<typeof result2, expected2>>;

// Five promises
const result3 = waitForAll([
  Promise.resolve(3),
  Promise.resolve("Hello" as const),
  Promise.resolve(true),
  Promise.resolve({ key: "value" }),
  Promise.resolve(["array"]),
]);
type expected3 = Promise<[number, "Hello", boolean, { key: string }, string[]]>;
type test3 = Expect<Equal<typeof result3, expected3>>;
