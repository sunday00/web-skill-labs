interface A {
  name: string
}

interface B {
  age: number
}

interface C extends A, B {
  canBark: boolean
}

export const human: C = {
  name: 'k',
  age: 22,
  canBark: false,
}
