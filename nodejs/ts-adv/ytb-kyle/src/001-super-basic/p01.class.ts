interface A {
  name: string
}

interface B {
  age: number
}

interface C extends A, B {
  canBark: boolean
}

const human: C = {
  name: 'k',
  age: 23,
  canBark: false,
}

export function runP01() {
  console.log('p01 : ', human)
}
