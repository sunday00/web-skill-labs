import { Equal, Expect } from '..'

//colors: "red", "orange" and "blue"

type LogLevel = 'error' | 'warning' | 'info'

// export type StatusColor<Level extends LogLevel> = Level extends 'error'
//   ? 'red'
//   : Level extends 'warning'
//     ? 'orange'
//     : Level extends 'info'
//       ? 'blue'
//       : never

export type StatusColor<Level extends LogLevel> = {
  error: 'red'
  warning: 'orange'
  info: 'blue'
}[Level]

type result1 = StatusColor<'error'>
type test1 = Expect<Equal<result1, 'red'>>

type result2 = StatusColor<'error' | 'warning'>
type test2 = Expect<Equal<result2, 'red' | 'orange'>>

type result3 = StatusColor<'warning' | 'info'>
type test3 = Expect<Equal<result3, 'orange' | 'blue'>>

type result4 = StatusColor<'error' | 'warning' | 'info'>
type test4 = Expect<Equal<result4, 'red' | 'orange' | 'blue'>>
