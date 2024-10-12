import { Equal, Expect } from '..'

declare function mergeObjects<
  // Infer `ObjectArray` as a tuple of objects:
  ObjectArray extends [{}, ...{}[]],
>(...objects: ObjectArray): MergeAll<ObjectArray>

export type MergeAll<Tuple, Accumulated = {}> = Tuple extends [infer First, ...infer Rest]
  ? MergeAll<Rest, Accumulated & First>
  : Accumulated

// Two objects
const result1 = mergeObjects({ name: 'Michel', age: 82 }, { childrenCount: 3 })
type Expected1 = { name: string; age: number; childrenCount: number }
type Test1 = Expect<Equal<typeof result1, Expected1>>

// Three objects
const result2 = mergeObjects(
  { protocol: 'https' as const },
  { domain: 'codelicks.com' },
  { path: '/advanced-typescript' },
)
type Expected2 = { protocol: 'https'; domain: string; path: string }
type Test2 = Expect<Equal<typeof result2, Expected2>>

// Five objects
const result3 = mergeObjects({ a: true }, { b: 2 }, { c: '4' }, { d: null }, { 2: 2n })
type Expected3 = { a: boolean; b: number; c: string; d: null; 2: bigint }
type Test3 = Expect<Equal<typeof result3, Expected3>>

// One object
const result4 = mergeObjects({ fileName: 'hello-world', extension: 'txt' })
type Expected4 = { fileName: string; extension: string }
type Test4 = Expect<Equal<typeof result4, Expected4>>
