// type BuildObject<T, E = null> = {
type BuildObject<T, E extends string | number | null = null> = {
  prop1: T
  prop2: E
}

type Test = BuildObject<string, number>

//Fix this error.
type Test2 = BuildObject<string> //{prop1: string, prop2: null}

export {}
