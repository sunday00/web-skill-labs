type CustomString = {
  value: string
}

type GenericT<T extends CustomString> = {
  name: T
}

type StringAndAge = {
  age: number
} & CustomString

const o: GenericT<StringAndAge> = {
  name: {
    value: 'hello',
    age: 3,
  },
}

export function runP03() {
  console.log(o.name.value)
}
