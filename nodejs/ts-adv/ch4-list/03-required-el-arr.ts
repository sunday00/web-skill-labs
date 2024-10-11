const nullableArr: Array<string> = []

type RequiredNotEmptyArray<T> = [T, ...T[]]
// const requiredArr: RequiredNotEmptyArray<string> = [] // err
const requiredArr1: RequiredNotEmptyArray<string> = ['a'] // err
const requiredArr2: RequiredNotEmptyArray<string> = ['a', 'b'] // err
