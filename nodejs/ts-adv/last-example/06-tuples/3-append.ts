// type Append<T extends any[], E> = T extends [...infer U] ? [...U, E] : never
type Append<T extends any[], E> = [...T, E]

type test1 = Append<[1, 2, 3], 4>
type test2 = Append<[], 1>
