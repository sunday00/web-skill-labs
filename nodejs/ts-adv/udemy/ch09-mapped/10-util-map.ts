const entries = Object.entries({ name: 'Jack', age: 23 })
const user = Object.fromEntries(entries)

type Entries<Obj> = {
  [Key in keyof Obj]: [Key, Obj[Key]]
}[keyof Obj]
type EE = Entries<{ name: 'Jack'; age: 23 }>

type FromEntries<E extends [any, any]> = {
  [K in E as K[0]]: K[1]
}
type FE = FromEntries<['name', 'Jack'] | ['age', 23]>
const fe: FE = {
  name: 'Jack',
  age: 23,
}
