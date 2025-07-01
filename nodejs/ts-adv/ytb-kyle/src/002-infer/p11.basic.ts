type Ge<T> = T extends Array<infer A> ? A : never

const a: Ge<['a', 'b']> = 'a'

const _t = ['abc', 'def', 1]
const b: Ge<typeof _t> = '1'
const c: Ge<typeof _t> = 3

const _t2 = 'asd'
let b2: Ge<typeof _t2> = '' as never

export function run11() {
  console.log(a, b, c, b2)
}
