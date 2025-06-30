type GenericT<T> = {
  prop: T extends string ? string : number
}

type Custom = {}

const o1: GenericT<Custom> = {
  prop: 1,
}

const o2: GenericT<string> = {
  prop: '1',
}

export function runP05() {
  console.log(o1.prop, o2.prop)
}
