export interface Obj {
  username: string
  email: string
  role: number
}

type ObjSetters<O> = {
  [K in keyof O as `set${K extends string ? Capitalize<K> : never}`]: () => void
}

/*
{
  setUsername: () => void;
  setEmail: () => void;
  setRole: () => void;
}
*/

type O = ObjSetters<Obj>
const o: O = {
  setUsername() {},
  setEmail() {},
  setRole() {},
}
