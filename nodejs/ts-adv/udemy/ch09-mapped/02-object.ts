type Person = {
  name: string
  age: number
}

type UpdatedPerson = {
  [P in keyof Person]: Person[P]
}
