type n = number & string // <- this type meas 'never'

let nn: n

// nn = 4 // TS2322: Type number is not assignable to type never

function fail(): never {
  throw new Error('failed...')
}

const err: never = fail()

const st: string = fail() // ??
const n: number = fail() // ??
