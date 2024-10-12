import { Equal, Expect } from "..";

/**
 * Define the `selectColumns` function to accept a Table,
 * a list of column names, and return a table that
 * includes only columns with these names.
 */
type DataColumn = { name: string; values: unknown[] };

declare function selectColumns<
  // Infer `TableColumns` as a tuple containing columns:
  TableColumns extends [DataColumn, ...DataColumn[]],
  // Infer `ColumnNames` as a union of string literal type:
  ColumnNames extends string
>(
  table: TableColumns,
  columnNames: ColumnNames[]
): PickColumns<TableColumns, ColumnNames>;

// This is a "filter" loop!
type PickColumns<Table, NameUnion> =
  // 1. Split the list:
  Table extends [infer First, ...infer Rest]
    ? // 2. Check if `First.name` is assignable to `NameUnion`:
      First extends { name: NameUnion }
      ? // 3. Keep the column if it is:
        [First, ...PickColumns<Rest, NameUnion>]
      : // 4. Exclude the column if it isn't:
        PickColumns<Rest, NameUnion>
    : // If the list is empty, return an empty list:
      [];

declare const personTable: [
  { name: "firstName"; values: string[] },
  { name: "lastName"; values: string[] },
  { name: "age"; values: number[] }
];

const result1 = selectColumns(personTable, ["age"]);
type test1 = Expect<Equal<typeof result1, [{ name: "age"; values: number[] }]>>;

const result2 = selectColumns(personTable, ["firstName", "lastName"]);
type test2 = Expect<
  Equal<
    typeof result2,
    [
      { name: "firstName"; values: string[] },
      { name: "lastName"; values: string[] }
    ]
  >
>;

const result3 = selectColumns(personTable, []);
type test3 = Expect<Equal<typeof result3, []>>;
