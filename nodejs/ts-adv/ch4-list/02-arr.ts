type Keywords = string[]

type Person = { id: number; name: string }
type Users = Array<Person>

type Binaries = (0 | 1)[]

type BoolArr = boolean[]
type Content = BoolArr[number] // this is just bool
// How to be useful?

type ThirdPartiesComplicated<T> = T extends Array<number> ? number[] : string[]
type NumberOrString1 = ThirdPartiesComplicated<number[]>[number]
type NumberOrString2 = ThirdPartiesComplicated<string[]>[number]

// example
// make type for a[0] = 8|9
// and a[1] ~ a[n] = any number
// ends with two string
type Special = [8 | 9, ...number[], string, string]
// const sp: Special = [10, 1, 2, 4, 5, 6, 7, 'ad']
// const sp: Special = [9, 1, 2, 4, 5, 6, 7, 'ad']
const sp: Special = [9, 1, 2, 4, 5, 6, '7', 'ad']

type L100 = Omit<Array<number>, 'length'> & { length: 100 } // this is works!
const a100: L100 = [
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5,
  6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5,
  6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
]
type Special2 = [8 | 9, ...L100, string, string]
const special2: Special2 = [8, 1, 3, 's', '7'] // unfortunately, this is just like same sp
