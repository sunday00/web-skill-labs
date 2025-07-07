type O2 = {
  readonly name: string
  age: number
}

type O3 = {
  age?: number
}

type N2<T> = {
  -readonly [P in keyof T]: T[P]
}

type N3<T> = {
  [P in keyof T]-?: T[P]
}

const a2: N2<O2> = {
  name: '1',
  age: 1,
}

a2.name = 'asd'

const a3: N3<O3> = {
  age: 1, // mandatory
}
