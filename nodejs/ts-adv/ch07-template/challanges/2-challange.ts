import { Equal, Expect } from '..'

/**
 * Create a `IsUppercase` generic that returns
 * true if the input string is in all uppercase letters and
 * false otherwise.
 *
 * You should not need recursion for this task.
 */

type IsUppercase<Str extends string> = Str extends Uppercase<Str> ? true : false

type result1 = IsUppercase<'HELLO'>
type test1 = Expect<Equal<result1, true>>

type result2 = IsUppercase<'Hello'>
type test2 = Expect<Equal<result2, false>>

type result3 = IsUppercase<'I am JAck'>
type test3 = Expect<Equal<result3, false>>

type result4 = IsUppercase<'WHOISJACKANYWAY'>
type test4 = Expect<Equal<result4, true>>
