import { Equal, Expect } from "..";

type ExtractValues<T> = T extends object ? T[keyof T] : never;
//                      👆
// We distribute T, and then access all its properties using `T[keyof T]`.

type result1 = ExtractValues<{ a: "value a" }>;
type test1 = Expect<Equal<result1, "value a">>;

type result2 = ExtractValues<{ a: "value a" } | { b: "value b" }>;
type test2 = Expect<Equal<result2, "value a" | "value b">>;

type result3 = ExtractValues<
  { a: string; b: number } | { b: boolean; c: bigint }
>;
type test3 = Expect<Equal<result3, string | number | boolean | bigint>>;
