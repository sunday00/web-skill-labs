type ReactState<T> = [T, (state: T) => void]

type Tuple1 = ['Kimera', 33, true]
type V = Tuple1[number] // this means can be every type from Tuple1
// === type V = Kimera | 33 | true
// because tuple's every index is number.
const v: V = 'Kimera'

type keys = keyof Tuple1
const k: keys = 'map' // keyof tuple type can be every normal list's methods, property too.
// k can be , so, map, length, forEach...

type T1 = ['a', 1, true]
type T2 = ['b', undefined, Function]
type T3 = [...T1, ...T2] // Yes. tuple type can be combined like array.

type Named = [name: string, age: number] // this is just for code hint, not any effect, but, that's enough.
const user: Named = ['lisa-su', 77]
