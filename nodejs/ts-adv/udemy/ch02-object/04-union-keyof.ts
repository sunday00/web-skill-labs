type A = {
  name: string
}

type B = {
  age: number
}

type C = A & B

const c: C = { name: 'a', age: 1 }

type AA = {
  name: string
  isAdmin: boolean
}

type BB = {
  age: number
  hasStock: boolean
}

type KA = keyof AA
type KB = keyof BB
type CC = AA | BB
type KC = keyof CC // this type is never.
// because KEY is maybe almost string, string is not possible to be 'name' and other whatever in sametime.

type DD = AA & BB
type KD = keyof DD // this type is 'name' | 'isAdmin' | 'age' | 'hadStock'

const kd: KD = 'name'
