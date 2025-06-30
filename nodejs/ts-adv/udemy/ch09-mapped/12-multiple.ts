type Person2 = {
  name: string
  email?: string
}

type Original = { key1: 1; key2: 2; key3: 3 }
// type Expected = {key1?: 1, key2?: 2, key3: 3}

type PartialOptional<T, OptionalK> = {
  [K in Extract<keyof T, OptionalK>]?: T[K]
} & {
  [K in Exclude<keyof T, OptionalK>]: T[K]
}

const allRequired = {
  a: 1,
  b: 2,
  c: 3,
}

const abOptionalCRequired: PartialOptional<typeof allRequired, 'a' | 'b'> = {
  a: 1,
  c: 4,
}
