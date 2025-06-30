// type MapToUnion<T> = T extends unknown ? Modify<T> : never

type Duplicate<T> = [T, T]
type D1 = Duplicate<1 | 2 | 3>

type DDuplicate<T> = T extends unknown ? [T, T] : never
type D2 = DDuplicate<1 | 2 | 3>

const typeDuplicated: D1 = [1, 3] // fine

// const valueDuplicated: D2 = [1, 3] // not OK
const valueDuplicated: D2 = [2, 2] // value must same
