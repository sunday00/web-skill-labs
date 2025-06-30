type Color<I> = I extends 0
  ? 'green'
  : I extends 1
    ? 'red'
    : I extends 2
      ? 'blue'
      : 'white'

type SimpleColor<N extends 0 | 1 | 2 | 3> = {
  0: 'green'
  1: 'red'
  2: 'blue'
  3: 'white'
}[N]
