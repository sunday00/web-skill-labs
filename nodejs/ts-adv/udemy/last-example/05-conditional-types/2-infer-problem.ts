export interface CustomInterface<E, C, P> {
  retrieveE: () => E
  fetchC: () => C
  acquireP: () => P
}
type NewType = CustomInterface<'E value', 'C value', 'P value'>

type Values<T> = T extends { acquireP: () => infer P } ? P : never
type Values2<T> = T extends CustomInterface<any, any, infer A> ? A : never
type Values3<T> = T extends CustomInterface<any, any, any> ? ReturnType<T['acquireP']> : never

type test = Values<NewType> // "P value"
type test2 = Values2<NewType> // "P value"
type test3 = Values3<NewType> // "P value"
