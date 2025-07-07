type O = {
  name: string
  age: number
}

type N<T> = {
  // [P in keyof T]: T[P]
  [P in keyof T]: T[P] extends string ? number : T[P]
}

const a: N<O> = {
  // name: '1',
  name: 1,
  age: 1,
}
