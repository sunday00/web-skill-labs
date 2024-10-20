import { Equal, Expect } from '..'

/**
 * Note: The list of "falsy" values is
 * false, null, 0, "", and undefined.
 */
type FalsyValues = false | null | 0 | undefined | ''

declare function filterFalsy<T>(array: T[]): Exclude<T, FalsyValues>[]

let result1 = filterFalsy([1, 2, null, 3, undefined, 4])
type test1 = Expect<Equal<typeof result1, number[]>>

let result2 = filterFalsy([undefined, 0 as const, 'a', '', 'b', 'c', 'd'])
type test2 = Expect<Equal<typeof result2, string[]>>

let result3 = filterFalsy([...([1, undefined, true, '', 'hello', null] as const)])
type test3 = Expect<Equal<typeof result3, (1 | true | 'hello')[]>>
