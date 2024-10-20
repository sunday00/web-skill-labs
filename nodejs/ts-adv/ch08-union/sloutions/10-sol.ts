import { Equal, Expect } from "..";

type ExtractPaths<T> =
  // If `T` is an object with string keys
  T extends Record<string, unknown>
    ? // Assign the union `keyof T` to a variable `K`
      keyof T extends infer K
      ? // Distribute `K`.
        // For each member in `K`:
        K extends string
        ? // Concatenate `K` with the rest of the path
          K | `${K}.${ExtractPaths<T[K]>}`
        : // Unreachable branch (`K` is always a string)
          never
      : // Unreachable branch (`extends infer` is always truthy)
        never
    : // If `T` isn't a record, return an empty type
      never;

// Alternative implementation using a Mapped Type
type ExtractPathsMapped<T> = {
  [K in keyof T]: K | `${Extract<K, string>}.${ExtractPaths<T[K]>}`;
}[keyof T];

// Test cases to verify the functionality
type testCase1 = ExtractPaths<{ name: string; age: number }>;
type result1 = Expect<Equal<testCase1, "name" | "age">>;

type testCase2 = ExtractPaths<{ user: { name: { first: string } } }>;
type result2 = Expect<
  Equal<testCase2, "user" | "user.name" | "user.name.first">
>;

type testCase3 = ExtractPaths<{
  user: { name: { first: string; last: Error } };
}>;
type result3 = Expect<
  Equal<testCase3, "user" | "user.name" | "user.name.first" | "user.name.last">
>;

type testCase4 = ExtractPaths<{
  name: string;
  age: number;
  team: { name: number; membersCount: number };
}>;
type result4 = Expect<
  Equal<testCase4, "name" | "age" | "team" | "team.name" | "team.membersCount">
>;
