export type GetNameValue<T> = T extends { name: infer E } ? E : never

type Test = GetNameValue<{ name: 'blabla' }> // "blabla"
type Test2 = GetNameValue<{ name: { key: 123 } }> // {key: 123}

type GetNameValue2<T> = T extends { [K in keyof T]: any } ? T[Extract<keyof T, 'name'>] : never
type G = GetNameValue2<{ name: 'avx'; age: 12 }>
