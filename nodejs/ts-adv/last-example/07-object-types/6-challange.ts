import { Equal, Expect } from '..'

/**
 * Type the "extractValue" function to infer its return type
 * from the object's type and the "path" string.
 */

declare function extractValue<T, S extends string>(obj: T, path: S): GetFromPath<T, S>

type GetFromPath<T, S> = RecursiveGet<T, ParsePath<S>>

type ParsePath<
  Path,
  Properties extends string[] = [],
  CurrentProp extends string = '',
> = Path extends `${infer F}${infer R}`
  ? F extends '.' | '[' | ']'
    ? ParsePath<R, [...Properties, ...(CurrentProp extends '' ? [] : [CurrentProp])]>
    : ParsePath<R, Properties, `${CurrentProp}${F}`>
  : [...Properties, ...(CurrentProp extends '' ? [] : [CurrentProp])]

type RecursiveGet<T, Properties> = Properties extends [infer F, ...infer R]
  ? F extends keyof T
    ? RecursiveGet<T[F], R>
    : [F, T] extends [`${number}`, any[]]
      ? RecursiveGet<As<T, any[]>[number], R>
      : undefined
  : T

type As<T, F> = T extends F ? T : never

// Example objects and type checks
declare const example1: { a: { b: { c: string } } }
const result1 = extractValue(example1, 'a.b.c')
type check1 = Expect<Equal<typeof result1, string>>

declare const example2: { author: { friends: [{ age: 29 }] } }
const result2 = extractValue(example2, 'author.friends[0].age')
type check2 = Expect<Equal<typeof result2, 29>>

declare const example3: { author: { friends: [undefined, { name: 'James' }] } }
const result3 = extractValue(example3, 'author.friends[1].name')
type check3 = Expect<Equal<typeof result3, 'James'>>

declare const example4: [1, 2, [3, [{ title: '❤️' }]]]
const result4 = extractValue(example4, '[2][1][0].title')
type check4 = Expect<Equal<typeof result4, '❤️'>>
