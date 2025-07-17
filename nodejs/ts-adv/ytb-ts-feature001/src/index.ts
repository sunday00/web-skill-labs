type UserType = {
  person: { firstname: string; lastname: string }
  company: { name: string }
}

// function mockUser(type: 'person'): UserType['person']
// function mockUser(type: 'company'): UserType['company']
// function mockUser(type: 'person' | 'company'): UserType['person' | 'company'] {
//   switch (type) {
//     case 'person':
//       return { firstname: 'kin', lastname: 'go' }
//     case 'company':
//       return { name: 'apple' }
//     // default: return undefined
//   }
// }

// function mockUser(type: 'person'): UserType['person']
// function mockUser(type: 'company'): UserType['company']
function mockUser<T extends keyof UserType>(type: T): UserType[T] {
  switch (type) {
    case 'person':
      return { firstname: 'kin', lastname: 'go' }
    case 'company':
      return { name: 'apple' }
    // default: return undefined
  }
}

console.log(mockUser('company'))
