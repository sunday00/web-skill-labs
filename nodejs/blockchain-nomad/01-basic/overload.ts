// type Add = {
//   (x: number, y: number): number
//   (x: number, y: string): number
// }
//
// const add: Add = (x: number, y: number | string) => {
//   if (typeof x === 'string') return x
//   return Number(y)
// }
//

type Add = {
  (x: number, y: number): number
  (x: number, y: number, z: number): number
}

const add: Add = (a, b, c?: number) => {
  return a + b + (c ?? 0)
}
