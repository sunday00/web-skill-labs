import { Equal, Expect } from '..'

/**
  Input:
    list: An array of items of type Item.
    condition: A type guard function that determines whether an item is of a specific type.
 * 
 */

/**
  Output:
      A tuple with two arrays:
          The first array contains items that satisfy the type guard condition.
          The second array contains items that do not satisfy the type guard condition .
 * 
 */

declare function divideArray<T, F extends T>(
  list: T[],
  condition: (value: T) => value is F,
): [F[], Exclude<T, F>[]]

const example1 = divideArray([1, 2, 'N/A', 7, 'oops'], (x): x is number => typeof x === 'number')
type test1 = Expect<Equal<typeof example1, [number[], string[]]>>

const example2 = divideArray([true, false, 1, true, 0], (x): x is boolean => typeof x === 'boolean')
type test2 = Expect<Equal<typeof example2, [boolean[], number[]]>>

const example3 = divideArray(
  ['value', 'onChange', 'onSubmit', 'valid', 'focused'],
  (x): x is `on${string}` => x.startsWith('on'),
)
type test3 = Expect<Equal<typeof example3, [`on${string}`[], string[]]>>
