import { Equal, Expect } from "..";

type TableColumn = { name: string; values: unknown[] };

declare function getColumnValues<
  // Define `Table` as a tuple containing columns:
  Table extends [TableColumn, ...TableColumn[]],
  // Define `ColName` as a string literal type:
  ColName extends string
>(
  table: Table,
  columnName: ColName
): Extract<Table[number], { name: ColName }>["values"];

// Example table definition
declare const exampleTable: [
  { name: "firstName"; values: string[] },
  { name: "lastName"; values: string[] },
  { name: "age"; values: number[] }
];

// Test cases to verify the functionality
const result1 = getColumnValues(exampleTable, "age");
type test1 = Expect<Equal<typeof result1, number[]>>;

const result2 = getColumnValues(exampleTable, "firstName");
type test2 = Expect<Equal<typeof result2, string[]>>;

// Test with variable column name
declare const columnName: "firstName" | "age";
const result3 = getColumnValues(exampleTable, columnName);
type test3 = Expect<Equal<typeof result3, string[] | number[]>>;
