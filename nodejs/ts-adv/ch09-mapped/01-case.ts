type ApiResponse = {
  product_name: 'something'
  shop: {
    shop_name: 'some shop'
  }
}

type Camelize<T> = {
  [K in keyof T as CamelCase<K>]: T[K] extends object ? Camelize<T[K]> : T[K]
}

type CamelCase<S> = S extends `${infer P}_${infer R}`
  ? `${Lowercase<P>}${Capitalize<CamelCase<R>>}`
  : S

type CamelizedApi = Camelize<ApiResponse>

const api: CamelizedApi = {
  productName: 'something',
  shop: {
    shopName: 'some shop',
  },
}

type Alphabet = 'x' | 'y' | 'z'
type MappedTypeExample = {
  [L in Alphabet]: L
}

type TuppledTypeExample = {
  [L in Alphabet]: [L, L]
}

type StringTypeExample = {
  [L in Alphabet]: `L is ${L}`
}
