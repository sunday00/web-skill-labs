type CustomString = {
  value: string
}

type GenericT<T> = T extends CustomString
  ? {
      name: T
    }
  : {
      name: boolean
    }

// type StringAndAge = {
//   age: number
// } & CustomString

type StringAndAge = {
  age: number
} & false

const o: GenericT<StringAndAge> = {
  // name: {
  //   value: 'hello',
  //   age: 3,
  // },
  name: false,
}

export function runP04() {
  // console.log(o.name.value)
  console.log(o.name)
}
