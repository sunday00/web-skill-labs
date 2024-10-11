type SampleFirst<T> = T extends [infer F, ...any[]] ? F : never
type SampleRemains<T> = T extends [any, ...infer Rest] ? Rest : []
type SampleFirstLast<T> = T extends [infer F, ...any[], infer L] ? [F, L] : []

type IsEqual = (a: number, b: number) => boolean

type IsEqual2 = {
  inputs: [a: number, b: number]
  outputs: boolean
}
type Parameters2<F> = F extends (...params: infer P) => any ? P : never
type P2 = Parameters2<IsEqual>

type Returns<F> = F extends (...params: any[]) => infer R ? R : never
type R2 = Returns<IsEqual>
