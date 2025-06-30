type FirstName = 'sun'
type LastName = 'day'
type FullName = `${FirstName} ${LastName}`

type O = { '0': 1 }
type Fet<T, K extends keyof T> = T[K]

// type R1 = Fet<O, 0> // impossible
type R1 = Fet<O, `${0}`>

type Chuck = 'Chuck'
type StartWith<N extends string> = `${N} ${string}`

const n1: StartWith<Chuck> = 'Chuck hard'
const n2: StartWith<Chuck> = 'Chuck soft'
// const n3: StartWith<Chuck> = "Tube hard"

type Theme = 'dark' | 'light'
type Sizes = 's' | 'm' | 'l' | 'xl'
type CssClasses = `${Theme}-${Sizes}`

type Horizon = 'e' | 'w'
type Vertical = 't' | 'b'
const getDirection = (horizon: Horizon, vertical: Vertical) => {
  const direction = `${horizon}-${vertical}` as const

  switch (direction) {
    case 'e-t':
      return 'ET'
    case 'e-b':
      return 'EB'
    case 'w-t':
      return 'WT'
    case 'w-b': // try to comment this block
      return 'WB' // try to comment this block

    default:
      ensureExhaustive(direction)
  }
}

function ensureExhaustive(input: never): never {
  throw new Error('ensureExhaustive')
}
