import { Equal, Expect } from '..'

type ExtractPaths<T> =
  T extends Record<string, unknown>
    ? keyof T extends infer K
      ? K extends string
        ? K | `${K}.${ExtractPaths<T[K]>}`
        : never
      : never
    : never

type ExtractPathsMapped<T> = {
  [K in keyof T]: K | `${Extract<K, string>}.${ExtractPaths<T[K]>}`
}[keyof T]

// Test cases to verify the functionality
type testCase1 = ExtractPaths<{ name: string; age: number }>
type result1 = Expect<Equal<testCase1, 'name' | 'age'>>

type testCase2 = ExtractPaths<{ user: { name: { first: string } } }>
type result2 = Expect<Equal<testCase2, 'user' | 'user.name' | 'user.name.first'>>

type testCase3 = ExtractPaths<{
  user: { name: { first: string; last: Error } }
}>
type result3 = Expect<Equal<testCase3, 'user' | 'user.name' | 'user.name.first' | 'user.name.last'>>

type testCase4 = ExtractPaths<{
  name: string
  age: number
  team: { name: number; membersCount: number }
}>
type result4 = Expect<Equal<testCase4, 'name' | 'age' | 'team' | 'team.name' | 'team.membersCount'>>
