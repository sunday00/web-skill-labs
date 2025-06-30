type A = {
  name: string
}

type B = {
  age: number
}

type C = A &
  B & {
    canBark: boolean
  }

const human: C = {
  name: 'k',
  age: 23,
  canBark: false,
}

export function runP02() {
  console.log('p02 : ', human)
}
