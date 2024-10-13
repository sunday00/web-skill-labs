import { Equal, Expect } from "..";

type SnakeToCamelCase<Str> = Str extends `${infer Start}_${infer Rest}`
  ? `${Start}${Capitalize<SnakeToCamelCase<Rest>>}`
  : Str;

// it should not change string that has no underscore.
type result1 = SnakeToCamelCase<"hello">;
type test1 = Expect<Equal<result1, "hello">>;

// single underscore
type result2 = SnakeToCamelCase<"hello_world">;
type test2 = Expect<Equal<result2, "helloWorld">>;

// multiple underscores
type result3 = SnakeToCamelCase<"hello_code_licks_academy">;
type test3 = Expect<Equal<result3, "helloCodeLicksAcademy">>;
