import { Equal, Expect } from '..'

/*
 ["x", 0] ---> [["x",0], [0,1]]

 * * Hint: you will need to use T["length"]
 * to generate indices.
 */

type MapWithIndex<Tuple extends any[], Output extends any[] = []> = Tuple extends [
  infer First,
  ...infer Rest,
]
  ? MapWithIndex<Rest, [...Output, [First, Output['length']]]>
  : Output

// Example usage and tests
type example1 = MapWithIndex<['x']>
type test1 = Expect<Equal<example1, [['x', 0]]>>

type example2 = MapWithIndex<['x', 'y']>
type test2 = Expect<Equal<example2, [['x', 0], ['y', 1]]>>

type example3 = MapWithIndex<['x', 'y', 'z']>
type test3 = Expect<Equal<example3, [['x', 0], ['y', 1], ['z', 2]]>>
