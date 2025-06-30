type PersonForm = {
  name: { value: string; isValid: boolean }
  email?: { value: string; isValid: boolean }
  readonly phone: { value: string; isValid: boolean }
}

type ExtractValue<V> = V extends { value: infer V } ? V : never

type Transform<F> = {
  [Field in keyof F]: ExtractValue<F[Field]>
}

type res = Transform<PersonForm>

const r: res = {
  name: 'hello',
  phone: '111',
} // still optional email. // type transforming preserve metadata

// r.phone = ''
