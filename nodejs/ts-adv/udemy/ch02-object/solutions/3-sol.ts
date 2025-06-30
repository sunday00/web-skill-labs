import { Equal, Expect } from '../challanges'

// Utility type to exclude the 'id' property from an object type
export type ExcludeId<T> = Omit<T, 'id'>

// Test cases to validate the ExcludeId utility type
type result1 = ExcludeId<{
  id: number
  name: string
  age: unknown
}>
type test1 = Expect<Equal<result1, { name: string; age: unknown }>>

type result2 = ExcludeId<{
  id: number
  title: string
  content: string
}>
type test2 = Expect<Equal<result2, { title: string; content: string }>>
