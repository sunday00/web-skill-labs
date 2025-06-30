type ForFrontend<T> = {
  [K in Exclude<keyof T, `secret${string}`>]: T[K]
}

type ForFrontend2<T> = Omit<T, `secret${string}`>
