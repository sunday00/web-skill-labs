import { Equal, Expect } from '../solutions'

export function combine<X, Y>(obj1: X, obj2: Y): X & Y {
  return { ...obj1, ...obj2 }
}

const result1 = combine({ name: 'Alice' }, { age: 30 })
type test1 = Expect<Equal<typeof result1, { name: string } & { age: number }>>

const result2 = combine({ greeting: 'Hi' }, {})
type test2 = Expect<Equal<typeof result2, { greeting: string }>>

const result3 = combine({}, { greeting: 'Hi' })
type test3 = Expect<Equal<typeof result3, { greeting: string }>>

const result4 = combine({ x: 1, y: 2 }, { z: 3, w: 4 })
type test4 = Expect<
  Equal<typeof result4, { x: number; y: number } & { z: number; w: number }>
>
