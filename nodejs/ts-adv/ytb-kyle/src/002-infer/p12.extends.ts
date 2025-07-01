type Ge<T> = T extends Array<infer A> ? (A extends string ? number : never) : never

const b = ['an']

const a: Ge<typeof b> = 1

export function run12() {
  console.log(a)
}
