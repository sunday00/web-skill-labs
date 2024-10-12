import { Equal, Expect } from '..'

declare function waitForAll<
  // Infer `PromiseArray` as a tuple of promises:
  PromiseArray extends [Promise<any>, ...Promise<any>[]],
>(promises: PromiseArray): Promise<UnwrapAll<PromiseArray>>

type UnwrapAll<Promises> = Promises extends [Promise<infer V>, ...infer Rest]
  ? [V, ...UnwrapAll<Rest>]
  : []

// Two promises
const result1 = waitForAll([Promise.resolve(20), Promise.resolve('Hello' as const)])
type expected1 = Promise<[number, 'Hello']>
type test1 = Expect<Equal<typeof result1, expected1>>

// Three promises
const result2 = waitForAll([Promise.resolve(true), Promise.resolve('!'), Promise.resolve({})])
type expected2 = Promise<[boolean, string, {}]>
type test2 = Expect<Equal<typeof result2, expected2>>

// Five promises
const result3 = waitForAll([
  Promise.resolve(3),
  Promise.resolve('Hello' as const),
  Promise.resolve(true),
  Promise.resolve({ key: 'value' }),
  Promise.resolve(['array']),
])
type expected3 = Promise<[number, 'Hello', boolean, { key: string }, string[]]>
type test3 = Expect<Equal<typeof result3, expected3>>
