export interface User {
  id: string
  username: string
  age: number
}

type TupleUnionOfUser = {
  [K in keyof User]: [K, User[K]]
}

// type Tuple<O> = [keyof O, O[keyof O]]
// type T = Tuple<User>

type Tuple<O> = TupleUnionOfUser[keyof User]
type T = Tuple<User>
