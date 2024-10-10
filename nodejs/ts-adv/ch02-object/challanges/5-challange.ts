import { Equal, Expect } from '.'

/**
 * The MergeAndOverride type takes two types A and B,
 * and returns a new type where the properties of B override
 * the properties of A.
 */
export type MergeAndOverride<A, B> = Omit<A, keyof B> & B

/**
 * Combines two objects, with properties from the second object
 * overriding those from the first object where keys overlap.
 */
const combineObjects = <A, B>(obj1: A, obj2: B): MergeAndOverride<A, B> => ({
  ...obj1,
  ...obj2,
})

// Override `id` property with a different type
type result1 = MergeAndOverride<{ name: string; id: number }, { id: string }>
// Expect result1 to be an object with properties: { name: string; id: string }
type test1 = Expect<Equal<result1, { name: string } & { id: string }>>

// Override `age` and `role` properties with specific values
type result2 = MergeAndOverride<
  { name: string; age: string; role: string },
  { age: 42; role: 'admin' }
>
// Expect result2 to be an object with properties: { name: string; age: 42; role: "admin" }
type test2 = Expect<
  Equal<result2, { name: string } & { age: 42; role: 'admin' }>
>

// No overlapping properties between the two types
type result3 = MergeAndOverride<{ name: string; id: number }, { age: number }>
// Expect result3 to be an object with properties: { name: string; id: number; age: number }
type test3 = Expect<
  Equal<result3, { name: string; id: number } & { age: number }>
>

// Using the combineObjects function to merge two objects
const result4 = combineObjects({ name: 'Bob', id: 4 }, { id: '3' })
// Expect result4 to be an object with properties: { name: string; id: string }
type test4 = Expect<Equal<typeof result4, { name: string } & { id: string }>>
