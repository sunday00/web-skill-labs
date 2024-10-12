import { Equal, Expect } from '..'

/**
 ** Hint: you will need to use T["length"]
 * to read the length of a tuple `T`.
 */
type TakeFirst<Tuple extends any[], N, Output extends any[] = []> = Output['length'] extends N
  ? Output
  : Tuple extends [infer First, ...infer Rest]
    ? TakeFirst<Rest, N, [...Output, First]>
    : Output

// Example usage and tests
type example1 = TakeFirst<[1, 2, 3], 2>
type test1 = Expect<Equal<example1, [1, 2]>>

type example2 = TakeFirst<[1, 2, 3], 1>
type test2 = Expect<Equal<example2, [1]>>

type example3 = TakeFirst<[1, 2, 3], 0>
type test3 = Expect<Equal<example3, []>>

type example4 = TakeFirst<[1, 2], 5>
type test4 = Expect<Equal<example4, [1, 2]>>
