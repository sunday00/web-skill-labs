import { Equal, Expect } from "..";

/**
 * Create a `SelectByCondition` generic type that takes a tuple,
 * an arbitrary `Condition` type, and filters out any element
 * that isn't assignable to `Condition`.
 */

type SelectByCondition<Tuple, Condition> = Tuple extends [
  infer First,
  ...infer Rest
]
  ? First extends Condition
    ? [First, ...SelectByCondition<Rest, Condition>]
    : SelectByCondition<Rest, Condition>
  : [];

// Example usage and tests
type example1 = SelectByCondition<[1, 2, "oops", 3, "hello"], number>;
type test1 = Expect<Equal<example1, [1, 2, 3]>>;

type example2 = SelectByCondition<["a", 1, "b", true, "c"], string>;
type test2 = Expect<Equal<example2, ["a", "b", "c"]>>;

type example3 = SelectByCondition<["hello", null, 42, {}, [], undefined], {}>;
type test3 = Expect<Equal<example3, ["hello", 42, {}, []]>>;
/*                                      ^      ^
/**  Note: strings and numbers are assignable to the `{}` object type.
*/
