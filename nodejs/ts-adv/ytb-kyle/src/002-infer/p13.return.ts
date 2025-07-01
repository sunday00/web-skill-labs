type CustomReturnType<T extends (...args: any[]) => any> = T extends (...args: any[]) => infer R
  ? R
  : never

type CustomAwaited<T> = T extends Promise<infer P> ? P : T

type A = ReturnType<typeof fetch>
type A2 = CustomReturnType<typeof fetch>
type A3 = Awaited<CustomReturnType<typeof fetch>>
type A4 = CustomAwaited<CustomReturnType<typeof fetch>>

export function run13() {}
