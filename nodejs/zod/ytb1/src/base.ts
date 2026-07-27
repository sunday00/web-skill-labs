import { z } from 'zod'

const UserSchema = z.object({
  username: z.string(),
})

const user = { username: 'sunday00' }
// const user = { username: 1 }

// console.log(UserSchema.parse(user))

// ------ using ts

// type User = {
//   username: string
// }

const UserSchema2 = z.object({
  username: z.string(),
})

type User = z.infer<typeof UserSchema2>

const user2: User = { username: 'sunday00' }

// console.log(UserSchema2.parse(user2))

// ------ non breakable error

const UserSchema3 = z.object({
  username: z.string(),
})

const user3 = { username: 1 }

// console.log(UserSchema3.safeParse(user3)) // error log but pass th
// console.log('hi')

// -------- more add property

const UserSchema4 = z.object({
  username: z.string(),
  age: z.number(),
  birth: z.date(),
  isDeveloper: z.boolean(),
})

const user4 /*: z.infer<typeof UserSchema4> */ = {
  username: 'sunday00',
  age: 3,
  birth: new Date('1986-01-01T00:00:00.000Z'),
  isDeveloper: 'true',
}

// console.log(UserSchema4.safeParse(user4).success)
// console.log(user4)

// -------- optional

const UserSchema5 = z.object({
  username: z.string(),
  age: z.number(),
  birth: z.date(),
  isDeveloper: z.boolean().optional(),
})

const user5: z.infer<typeof UserSchema5> = {
  username: 'sunday00',
  age: 3,
  birth: new Date('1986-01-01T00:00:00.000Z'),
}

// console.log(UserSchema5.safeParse(user5).success)
// console.log(user5)

// -------- amount validate

const UserSchema6 = z.object({
  username: z.string(),
  email: z.email().optional(),
  age: z.number().gt(10), // === min(10)
  birth: z.date().min(new Date('1990-01-01T00:00:00.000Z')),
  isDeveloper: z.boolean().optional(),
  etc: z.object().nullable(),
  profileImage: z.string().nullish(),
  isMember: z.boolean().default(true).nullish(),
})

const user6: z.infer<typeof UserSchema6> = {
  username: 'sunday00',
  email: 'abc&def.com',
  age: 3,
  birth: new Date('1986-01-01T00:00:00.000Z'),
  etc: null, // <---- can be null but property self is mandatory.
  // profile image can be null ALSO undefined
}

// console.log(UserSchema6.safeParse(user6)) // "code": "too_small", // "code": "invalid_format",

// --------- default
const hobbies = ['draw', 'guitar', 'walk'] as const

const UserSchema7 = z.object({
  username: z.string(),
  email: z.email().optional(),
  age: z.number().gte(1),
  birth: z.date().min(new Date('1950-01-01T00:00:00.000Z')),
  isMember: z.boolean().default(true).nullish(),
  task: z.literal(['dev', 'pm', 'design']),
  hobby: z.enum(hobbies).optional(), // almost literal similar enum
  // literal can set bool, num anything fixed value
  // enum can set string array
})

const user7: z.infer<typeof UserSchema7> = {
  username: 'sunday00',
  email: 'abc@def.com',
  age: 3,
  birth: new Date('1986-01-01T00:00:00.000Z'),
  task: 'dev',
}

console.log(UserSchema7.parse(user7))
