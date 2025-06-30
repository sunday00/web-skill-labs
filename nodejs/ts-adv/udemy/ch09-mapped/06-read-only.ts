type Person = {
  name: string
  age: number
}

type ROP = Readonly<Person>

const states = ['loading', 'success', 'info', 'warning', 'error'] as const

type States = (typeof states)[number]
