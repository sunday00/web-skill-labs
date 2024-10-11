import { Equal, Expect } from "..";

/**
 * Conditional type utility to choose between two branches based on a condition.
 * It checks if the condition is true and returns the corresponding branch type.
 */
type Conditional<Condition, TrueBranch, FalseBranch> = Condition extends true
  ? TrueBranch
  : FalseBranch;

// Example usage and tests

type example1 = Conditional<true, string, number>;
// Test case 1: If condition is true, choose `string`
type test1 = Expect<Equal<example1, string>>;

type example2 = Conditional<false, string, number>;
// Test case 2: If condition is false, choose `number`
type test2 = Expect<Equal<example2, number>>;

type example3 = Conditional<boolean, string, number>;
// Test case 3: If condition is a boolean, choose either `string` or `number`
type test3 = Expect<Equal<example3, string | number>>;
