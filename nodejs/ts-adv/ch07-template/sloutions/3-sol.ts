import { Equal, Expect } from "..";

type BeginsWith<
  Str extends string,
  Prefix extends string
> = Str extends `${Prefix}${string}` ? true : false;

type result1 = BeginsWith<"fetchData", "fetch">;
type test1 = Expect<Equal<result1, true>>;

type result2 = BeginsWith<"fetchData", "post">;
type test2 = Expect<Equal<result2, false>>;

type result3 = BeginsWith<"Hello, world!", "Hello">;
type test3 = Expect<Equal<result3, true>>;
