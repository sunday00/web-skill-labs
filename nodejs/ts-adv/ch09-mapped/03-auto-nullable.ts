type Person = {
  name: string
  age: number
}

type NullableValues<T> = {
  [K in keyof T]: T[K] | null
}

type PersonNullable = NullableValues<Person>

type NotNull<T> = Exclude<T, null>
type NotNullable<T> = {
  // [K in keyof T]: Exclude<T[K], null>
  [K in keyof T]: NotNull<T[K]>
}
type NotNullablePerson = NotNullable<{ name: string | null; email: string | null }>

const pp: NotNullablePerson = {
  name: 'asd',
  email: 'grewgfrew',
}

declare function withDefault<T>(obj: T): NotNullable<T>

const bad = { name: null as string | null }
const good = withDefault(bad)
