type Loop<L> = L extends [infer First, ...infer Rest] ? Loop<Rest> : undefined

type ExtractName<O> = O extends { name: infer Name } ? Name : 'no-name'
type ToNames<L> = L extends [infer First, ...infer Rest]
  ? [ExtractName<First>, ...ToNames<Rest>]
  : []
type Names = ToNames<[{ id: 1; name: 'Jack' }, { id: 2; name: 'Jane' }]>

type OnlyNumbers<L> = L extends [infer First, ...infer Rest]
  ? First extends number
    ? [First, ...OnlyNumbers<Rest>]
    : OnlyNumbers<Rest>
  : []
type numbers = OnlyNumbers<['a', 1, 2, 5, 'b', true, 9]>

type OnlyT<L, T> = L extends [infer First, ...infer Rest]
  ? First extends T
    ? [First, ...OnlyNumbers<Rest>]
    : OnlyNumbers<Rest>
  : []
type numbers2 = OnlyT<['a', 1, 2, 5, 'b', true, 9], number>
