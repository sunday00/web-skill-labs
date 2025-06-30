import { Equal, Expect } from '..'

type FlattenArray<Arr extends any[]> = Arr extends (infer A)[] ? Unwrap<A>[] : never
type Unwrap<A> = A extends (infer E)[] ? E : A

declare function flattenArray<A extends any[]>(arr: A): FlattenArray<A>

// Test cases
let result1 = flattenArray([1, 2, [3, 4]])
type test1 = Expect<Equal<typeof result1, number[]>>

// If the array is already flat, leave it unchanged
let result2 = flattenArray(['a', 'b', 'c', 'd'])
type test2 = Expect<Equal<typeof result2, string[]>>

// This should work when the types of values are different
let result3 = flattenArray(['a', 'b', [3, 4]]) // ["a", "b", 3, 4]
type test3 = Expect<Equal<typeof result3, (number | string)[]>>

// This should work when the types of values are different and nested
let result4 = flattenArray(['a', ['b'], [3, [4, 5]]]) // ["a", "b", 3 , [4, 5] ]
type test4 = Expect<Equal<typeof result4, (number | string | number[])[]>>
