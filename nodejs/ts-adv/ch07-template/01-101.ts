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
