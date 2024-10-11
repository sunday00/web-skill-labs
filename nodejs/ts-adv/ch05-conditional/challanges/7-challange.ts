import { Equal, Expect } from '..'

/**
 * Type utility to extract the last element of a tuple.
 * If the tuple is empty, it returns `never`.
 */
type GetLastElement<Tuple extends any[]> = Tuple extends [infer El, ...infer E]
  ? Tuple['length']
  : never

type result1 = GetLastElement<[1, 2, 3]>
type testCase1 = Expect<Equal<result1, 3>>

type result2 = GetLastElement<[1]>
type testCase2 = Expect<Equal<result2, 1>>

type result3 = GetLastElement<[]>
type testCase3 = Expect<Equal<result3, never>>
