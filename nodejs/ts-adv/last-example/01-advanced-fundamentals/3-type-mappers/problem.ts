export interface User {
  id: string
  age: number
  title?: string
}

/*
  YOUR JOB:
  1. Build a type based on User Interface, with immutable properties.
  2. In this new type, make all the optional properties to be mendatory. (Remove the ?)
*/

type UserWithNoOptionals = {
  [K in keyof User]-?: User[K] // -? means optional to required
}

const user: User = {
  id: '1',
  age: 1,
}

const user2: UserWithNoOptionals = {
  id: '2',
  age: 2,
  title: 'test',
}
