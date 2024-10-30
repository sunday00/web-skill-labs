interface Admin {
  level: number
  write_permission: boolean
}

interface User {
  username: string
  email: string
}

type Session<T extends 'admin' | 'student'> = {
  props: T extends 'admin' ? Admin : User
}

// let session: Session<"user">;
let session: Session<'admin'> = {
  props: {
    level: 3,
    write_permission: false,
  },
}

let stdSession: Session<'student'> = {
  props: {
    username: '',
    email: '',
  },
}
