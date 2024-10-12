// function extractColumnEx(table: { [key: string]: string }, columnName: string): string {
//   return table[columnName]
// }

declare function extractColumnEx<O extends { [P: string]: string }, P extends string>(
  ob: O,
  prop: P,
): O[P]

const vEx = extractColumnEx({ name: 'fdsfds' }, 'name')
console.log(vEx)
