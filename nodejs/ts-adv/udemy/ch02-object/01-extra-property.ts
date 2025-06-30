type Customer = {
  name: string
  age: number
  address: string
}

const Ashi: Customer = {
  name: 'Ashi',
  age: 11,
  address: 'Su-won ...',
  // tel: '000-000-0000' // impossible to additional prop in this way
}

let Tom: Customer
let customerInfo = {
  name: 'Tom',
  age: 18,
  address: 'Sung-nam ...',
  tel: '000-000-0000',
}
Tom = customerInfo // this is fine. in this way, ts not check all properties are correct
// ts checks only all type properties are 'exists'
