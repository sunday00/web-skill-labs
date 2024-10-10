type CustomerCh2 = {
  name: string
  age: number
  isAdmin: boolean
}

type Age = CustomerCh2['age']
type AgeOrIsAdmin = CustomerCh2['age' | 'isAdmin']
type AgeOrIsAdmin2 = CustomerCh2['age'] | CustomerCh2['isAdmin']

type CustomerCh2Keys = keyof CustomerCh2
type CustomerCh2Values = CustomerCh2[keyof CustomerCh2]

type ValuesOf<T> = T[keyof T]
type CustomerCh2Values2 = ValuesOf<CustomerCh2>
