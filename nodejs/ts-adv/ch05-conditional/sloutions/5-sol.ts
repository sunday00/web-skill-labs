import { Equal, Expect } from "..";

/**
 * Type utility to extract the resolved value type from a Promise.
 * If the input type is not a Promise, it returns the input type itself.
 */
type ExtractPromiseValue<T> = T extends Promise<infer ResolvedType>
  ? ResolvedType
  : T;

// Test cases to validate the ExtractPromiseValue type utility

type example1 = ExtractPromiseValue<Promise<"Hello">>;
type validation1 = Expect<Equal<example1, "Hello">>;

type example2 = ExtractPromiseValue<Promise<{ name: string; age: number }>>;
type validation2 = Expect<Equal<example2, { name: string; age: number }>>;

type example3 = ExtractPromiseValue<"NOT A PROMISE">;
type validation3 = Expect<Equal<example3, "NOT A PROMISE">>;
