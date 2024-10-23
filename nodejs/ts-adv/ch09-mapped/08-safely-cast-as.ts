const a = {} as { name: string }

console.log(a.name) // undefined

type SnakeToCamel<Str> = Str extends `${infer First}_${infer Last}`
  ? `${First}${SnakeToCamel<Capitalize<Last>>}`
  : Str

type CamelCaseKey<T> = {
  [Key in keyof T as SnakeToCamel<Key>]: T[Key]
}

type R = CamelCaseKey<{ some_long_key: number; another_key: string }>
