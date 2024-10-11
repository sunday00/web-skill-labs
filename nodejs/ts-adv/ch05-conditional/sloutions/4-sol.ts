import { Equal, Expect } from "..";

/**
 * Type utility to extract the type of the `name` property
 * from an object, or return `undefined` if the property does not exist.
 */
type ExtractNameType<T> = T extends { name: infer NameType }
  ? NameType
  : undefined;

// Test cases to validate the ExtractNameType type utility

type example1 = ExtractNameType<{ name: "Jack" }>;
type validation1 = Expect<Equal<example1, "Jack">>;

type example2 = ExtractNameType<{ name: string; age: number }>;
type validation2 = Expect<Equal<example2, string>>;

type example3 = ExtractNameType<{ age: number }>;
type validation3 = Expect<Equal<example3, undefined>>;

type example4 = ExtractNameType<{
  name: { firstName: string; lastName: string };
  age: number;
}>;
type validation4 = Expect<
  Equal<example4, { firstName: string; lastName: string }>
>;
