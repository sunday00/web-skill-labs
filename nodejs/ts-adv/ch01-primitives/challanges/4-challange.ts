export function convertToString(value: unknown): string {
  return typeof value === 'symbol' ? value.toString() : `${value}`
}

convertToString('example string') // ✅
convertToString(123) // ✅
convertToString(false) // ✅
convertToString(Symbol('dog')) // ✅
