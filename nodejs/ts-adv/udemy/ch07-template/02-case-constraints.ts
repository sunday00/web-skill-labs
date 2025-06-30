type User = {
  firstname: string
  lastname: string
  address: string
}

const user: User = {
  firstname: 'Kim',
  lastname: 'Man-su',
  address: 'Su-won ...',
}

type CreditCard = {
  name: Uppercase<`${User['firstname']} ${User['lastname']}`>
  address: Lowercase<User['address']>
}

type RegisterForm = {
  name: Capitalize<`${User['firstname']} ${User['lastname']}`>
  address: Uncapitalize<User['address']>
}

declare global {
  interface String {
    toCapitalize(each?: boolean): Capitalize<string>
    toUpperCase(): Uppercase<string>
    toLowerCase(): Lowercase<string>
  }
}

String.prototype.toCapitalize = function (each: boolean = false): Capitalize<string> {
  const execCapitalize = (s: string) => {
    const f = s[0]

    if (!f) return ''

    return f.toUpperCase() + s.slice(1, s.length)
  }

  if (each) {
    let t: string[] = []
    this.split(' ').forEach((s) => {
      t.push(execCapitalize(s))
    })
    return t.join(' ') as Capitalize<string>
  }

  return execCapitalize(this.toString()) as Capitalize<string>
}

console.log('happy song'.toCapitalize(true))

const step1: RegisterForm = {
  name: `${user.firstname.toCapitalize()} ${user.lastname}`,
  address: user.address.toLowerCase() as Uncapitalize<string>,
}

const step2: CreditCard = {
  name: `${user.firstname.toUpperCase()} ${user.lastname.toUpperCase()}`,
  address: user.address.toLowerCase(),
}

console.log(step1, step2)
