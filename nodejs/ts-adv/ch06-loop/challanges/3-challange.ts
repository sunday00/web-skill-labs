import { Equal, Expect } from '..'

type DataColumn = { name: string; values: unknown[] }

declare function selectColumns<
  // Infer `TableColumns` as a tuple containing columns:
  TableColumns extends [DataColumn, ...DataColumn[]],
  // Infer `ColumnNames` as a union of string literal type:
  ColumnNames extends string,
>(table: TableColumns, columnNames: ColumnNames[]): PickColumns<TableColumns, ColumnNames>

type PickColumns<Table, NameUnion> = Table extends [infer First, ...infer Rest]
  ? First extends { name: NameUnion }
    ? [First, ...PickColumns<Rest, NameUnion>]
    : PickColumns<Rest, NameUnion>
  : []

declare const personTable: [
  { name: 'firstName'; values: string[] },
  { name: 'lastName'; values: string[] },
  { name: 'age'; values: number[] },
]

const result1 = selectColumns(personTable, ['age'])
type test1 = Expect<Equal<typeof result1, [{ name: 'age'; values: number[] }]>>

const result2 = selectColumns(personTable, ['firstName', 'lastName'])
type test2 = Expect<
  Equal<
    typeof result2,
    [{ name: 'firstName'; values: string[] }, { name: 'lastName'; values: string[] }]
  >
>

const result3 = selectColumns(personTable, [])
type test3 = Expect<Equal<typeof result3, []>>
