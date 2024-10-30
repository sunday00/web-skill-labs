// export type ArrayOnly<T> = unknown;
export type ArrayOnly<T> = T extends unknown[] ? T : never

type StringsOrNumbersArray = ArrayOnly<number | string | string[] | number[]>
// string[] | number[]
