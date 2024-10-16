type ExtractKeys<T> = keyof T
type EK1 = ExtractKeys<{ a: 1; b: 2 } | { a: 3; c: 2 }>

type RealExtractKeys<T> = T extends unknown ? keyof T : never
type EK2 = RealExtractKeys<{ a: 1; b: 2 } | { a: 3; c: 2 }>
