import { Equal, Expect } from '../challanges'

// Utility type to extract the values of an object type
export type ExtractValues<T> = T[keyof T]

// Test cases to validate the ExtractValues utility type
type result1 = ExtractValues<{ x: number; y: string }>
type test1 = Expect<Equal<result1, number | string>>

type result2 = ExtractValues<{ foo: number; bar: string; baz: boolean }>
type test2 = Expect<Equal<result2, number | string | boolean>>

type result3 = ExtractValues<{}>
type test3 = Expect<Equal<result3, never>>

type result4 = ExtractValues<{ [K in string]: boolean }>
type test4 = Expect<Equal<result4, boolean>>
