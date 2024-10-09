// let arr: unknown
//
// arr = [1, 2, 3]
// arr.push(4) // impossible

let arr: Array<unknown> & unknown

arr = [1, 2, 3]
arr.push(4)

console.log(arr)
