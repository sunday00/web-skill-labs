type Setter<S> = S extends Set<infer V> ? V : never

type S1 = Setter<Set<string>>
type S2 = Setter<Set<number>>

type G1<A, B> = { content: A; children: B[] }
type Extractor1<S> = S extends G1<infer A, infer B> ? [A, B] : never
type e1 = Extractor1<G1<number, boolean>>

type SomeHeavy<T> = unknown
type Fn<T> = SomeHeavy<T> extends infer Result ? [Result, Result] : never
