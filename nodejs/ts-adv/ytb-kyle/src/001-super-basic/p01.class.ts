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
  age: 2,
  canBark: false,
}

console.log(human.name)
