import { Equal, Expect } from '.'

// A utility type to extract the keys of an object type
export type ExtractKeys<T> = keyof T

// Test cases to validate the ExtractKeys utility type
type result1 = ExtractKeys<{ x: number; y: string }>
type test1 = Expect<Equal<result1, 'x' | 'y'>>

type result2 = ExtractKeys<{ foo: number; bar: string; baz: unknown }>
type test2 = Expect<Equal<result2, 'foo' | 'bar' | 'baz'>>

type result3 = ExtractKeys<{}>
type test3 = Expect<Equal<result3, never>>

type result4 = ExtractKeys<{ [K in string]: boolean }>
type test4 = Expect<Equal<result4, string>>
