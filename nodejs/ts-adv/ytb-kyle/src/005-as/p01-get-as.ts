type OO = {
  name: string
  age: number
}

type NN<T extends Record<string, any>> = {
  [P in keyof T as `get${Capitalize<P & string>}`]: () => T[P]
}

const instance: NN<OO> = {
  getName: () => '',
  getAge: () => 1,
}
