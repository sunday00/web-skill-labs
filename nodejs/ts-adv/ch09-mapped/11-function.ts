type FunctionType = (...args: any[]) => any
// type RemoveFunction<T> = {
//   [K in keyof T]: T[K] extends FunctionType ? never : T[K]
// }

type Entries<Obj> = {
  [Key in keyof Obj]: [Key, Obj[Key]]
}[keyof Obj]

type FromEntries<E extends [any, any]> = {
  [K in E as K[0]]: K[1]
}
// type ExcludeFunctionEntries<Entry> = Entry extends [any, FunctionType] ? never : Entry
// type ExcludeFunctionEntries<Entry> = Exclude<Entry, [any, FunctionType]>
// type RemoveFunction<T> = FromEntries<ExcludeFunctionEntries<Entries<T>>>
type RemoveFunction<T> = FromEntries<Exclude<Entries<T>, [any, FunctionType]>>

type RemoveByType<T, RemoveT> = FromEntries<Exclude<Entries<T>, [any, RemoveT]>>

class Person {
  constructor(
    public name: string,
    public age: number,
  ) {}

  getWalk() {
    return this.name + ' is walking...'
  }
}

const person = new Person('cho', 33)

console.log(person.getWalk())

const noFuncPerson: RemoveFunction<Person> = {
  name: 'cho',
  age: 11,
}

const noFuncPerson2: RemoveByType<Person, number> = {
  name: 'kk',
  getWalk: () => {
    return noFuncPerson2.name + ' is walking...'
  },
}

console.log(noFuncPerson, noFuncPerson2.getWalk())

type PickByType<T, PickT> = FromEntries<Extract<Entries<T>, [any, PickT]>>

const numberOnlyPerson: PickByType<Person, number> = {
  // name: ''
  // getWalk: () => {}
  age: 122,
}

console.log(numberOnlyPerson)
