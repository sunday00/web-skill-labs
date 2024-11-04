// export type TupleToArray<T extends any[]> = T extends (infer U)[] ? U[] : never
export type TupleToArray<T extends any[]> = T[number][]

type test1 = TupleToArray<[1, 2, 3]>

type test2 = TupleToArray<[number, string]>

type test3 = TupleToArray<[]>

type test4 = TupleToArray<[1] | [2] | [3]>
