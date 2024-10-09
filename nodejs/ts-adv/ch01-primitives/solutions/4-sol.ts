export function convertToString(value: unknown): string {
  return typeof value === "symbol" ? value.toString() : String(value);
}

convertToString("example string"); // ✅
convertToString(123); // ✅
convertToString(false); // ✅
convertToString(Symbol("dog")); // ✅
