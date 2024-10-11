import { Equal, Expect } from "..";

/**
 * Type utility to get the value of a key from an object,
 * or return a default type if the key does not exist in the object.
 */
type FetchOrDefault<Key, Obj, Default> = Key extends keyof Obj
  ? Obj[Key]
  : Default;

/**
 * Function to get the value of a key from an object,
 * or return a default value if the key does not exist.
 *
 * @param key - The key to look up in the object
 * @param obj - The object to retrieve the value from
 * @param defaultValue - The default value to return if the key does not exist
 * @returns - The value from the object or the default value
 */
function fetchOrDefault<K extends string, O extends {}, D>(
  key: K,
  obj: O,
  defaultValue: D
): FetchOrDefault<K, O, D> {
  return (obj as any)[key] ?? defaultValue;
}

// Example usage and tests

const result1 = fetchOrDefault(
  "title",
  { title: "TypeScript Utility Types" },
  "hello"
);
// Test case 1: Key exists in the object, should return the value of the key
type test1 = Expect<Equal<typeof result1, string>>;

const result2 = fetchOrDefault(
  "oops",
  { title: "TypeScript Utility Types" },
  undefined
);
// Test case 2: Key does not exist, should return the default value (undefined)
type test2 = Expect<Equal<typeof result2, undefined>>;

const result3 = fetchOrDefault("age", { age: 29 }, 0);
// Test case 3: Key exists in the object, should return the value of the key
type test3 = Expect<Equal<typeof result3, number>>;

const result4 = fetchOrDefault("friends", { age: 29 }, ["Bob"]);
// Test case 4: Key does not exist, should return the default value (array of strings)
type test4 = Expect<Equal<typeof result4, string[]>>;
