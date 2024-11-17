let a = 'abc'
a = 'def'

const b: boolean = true

type Player = {
  readonly name: string
  age?: number
}

const p: Player = {
  name: 'Chk',
  age: 1,
}

// a.name = 'Chk'

const r: Record<number, number> = { 1: 1 } as const
