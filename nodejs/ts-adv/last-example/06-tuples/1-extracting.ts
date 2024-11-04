// type First<T extends any[]> = T extends [infer U, ...infer R] ? U : undefined
type First<T extends any[]> = T[0]

type test1 = First<[]>
type test2 = First<[string]>
type test3 = First<[2, 3, 4]>
type test4 = First<['a', 'b', 'c']>
