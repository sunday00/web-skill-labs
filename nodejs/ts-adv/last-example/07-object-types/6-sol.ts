import { Equal, Expect } from "..";

declare function extractValue<T, P extends string>(
  object: T,
  path: P
): ResolvePath<T, P>;

// Convert the path string into a list of properties,
// Then, recursively access properties on the input object.
type ResolvePath<ObjectType, PathType> = Traverse<ObjectType, Parse<PathType>>;

// Transform the path into a list of properties
// by checking if each character belongs to the
// `"." | "[" | "]"` union type. If it does, split
// the string at this position. Otherwise, keep going.
//
// This approach is similar to a `RemovePunctuation` generic,
// except we create a tuple type instead of a string here.
type Parse<
  PathType,
  Properties extends string[] = [],
  CurrentProperty extends string = ""
> =
  // Split the path after the first character
  PathType extends `${infer FirstChar}${infer RemainingPath}`
    ? // If the first character is a delimiter
      FirstChar extends "." | "[" | "]"
      ? // Add the CurrentProperty to `Properties` if it isn't
        // an empty string.
        Parse<
          RemainingPath,
          [
            ...Properties,
            ...(CurrentProperty extends "" ? [] : [CurrentProperty])
          ],
          ""
        >
      : // Otherwise, add the first character to the
        // current property name:
        Parse<RemainingPath, Properties, `${CurrentProperty}${FirstChar}`>
    : // If the input string is empty, return the list of
      // properties, with the current property appended to it.
      [...Properties, ...(CurrentProperty extends "" ? [] : [CurrentProperty])];

// Traverse the list of properties
// and get the corresponding value from the object.
// Recurse until the list is empty.
type Traverse<ObjectType, PropertiesList> = PropertiesList extends [
  infer FirstProperty,
  ...infer RemainingProperties
]
  ? FirstProperty extends keyof ObjectType
    ? Traverse<ObjectType[FirstProperty], RemainingProperties>
    : // Special case if `ObjectType` is an array
    // and the path is a number string.
    // In this case, read the array's inner
    // type using `array[number]`.
    [FirstProperty, ObjectType] extends [`${number}`, any[]]
    ? Traverse<Assume<ObjectType, any[]>[number], RemainingProperties>
    : undefined
  : ObjectType;

// The `Assume` type ensures TypeScript considers
// the first type parameter as a subtype of the second.
type Assume<A, B> = A extends B ? A : never;

// Example objects and type checks
declare const example1: { a: { b: { c: string } } };
const result1 = extractValue(example1, "a.b.c");
type check1 = Expect<Equal<typeof result1, string>>;

declare const example2: { author: { friends: [{ age: 29 }] } };
const result2 = extractValue(example2, "author.friends[0].age");
type check2 = Expect<Equal<typeof result2, 29>>;

declare const example3: { author: { friends: [undefined, { name: "James" }] } };
const result3 = extractValue(example3, "author.friends[1].name");
type check3 = Expect<Equal<typeof result3, "James">>;

declare const example4: [1, 2, [3, [{ title: "❤️" }]]];
const result4 = extractValue(example4, "[2][1][0].title");
type check4 = Expect<Equal<typeof result4, "❤️">>;
