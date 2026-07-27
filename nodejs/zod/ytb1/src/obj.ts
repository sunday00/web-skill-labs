import { z } from 'zod'

const hobbies = ['draw', 'guitar', 'walk'] as const

const UserSchema = z.object({
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

const user: z.infer<typeof UserSchema> = {
  username: 'sunday00',
  email: 'abc@def.com',
  age: 3,
  birth: new Date('1986-01-01T00:00:00.000Z'),
  task: 'dev',
}

// console.log(UserSchema.shape)
// console.log(UserSchema.shape.hobby)

// ------------ partial

const userInput: z.infer<Partial<typeof UserSchema>> = {
  username: 'sunday00',
}

const zUserInput = UserSchema.partial().parse(userInput)
console.log(zUserInput)

// ---> same thing. but real usage
const UserInputSchema = UserSchema.partial()
const userInput2: z.infer<typeof UserInputSchema> = {
  username: 'sunday00',
}
const zUserInput2 = UserInputSchema.parse(userInput2)

// ------------- other mapped type

const UserStringSchema = UserSchema.pick({ username: true, email: true })
const userStrings: z.infer<typeof UserStringSchema> = {
  username: 'monday00',
  email: 'monday@dot.com',
}

const UserExtended = UserStringSchema.extend({
  hasBeenRegistered: z.boolean(),
})
UserExtended.parse({ ...userStrings, hasBeenRegistered: false })

// -------- merge ---

function extendNested<
  S extends z.ZodObject<any>,
  K extends keyof S['shape'],
  E extends z.ZodRawShape,
>(schema: S, key: K, ext: E) {
  const inner = schema.shape[key] as z.ZodObject<any>
  return schema.extend({ [key]: inner.extend(ext) } as any)
}

const MetaSchema = z.object({
  name: z.string(),
  color: z.enum(['red', 'white', 'blue']),
  cost: z.number(),
})

const StatusSchema = z.object({
  outside: z.object({
    strain: z.boolean(),
    bumped: z.boolean(),
  }),
  inside: z.object({
    dirt: z.number().max(10).default(0),
    ran: z.number(),
  }),
  isUsed: z.boolean().default(false),
})

// const CarSchema = MetaSchema.extend(StatusSchema)
const CarSchema = MetaSchema.extend(StatusSchema.shape)
const carRaw: z.infer<typeof CarSchema> = {
  name: 'avante',
  color: 'blue',
  cost: 1000,
  outside: {
    strain: false,
    bumped: true,
  },
  inside: {
    dirt: 2,
    ran: 10_000,
  },
  isUsed: true,
}
const car = CarSchema.parse(carRaw)
console.log(car)

// ------------- keep schema & keep additional prop

const FruitSchema = z.object({ name: z.string(), cost: z.number() })
const bananaRaw = { name: 'banana', cost: 100, color: 'yellow' }
const banana = FruitSchema.parse(bananaRaw)
console.log(banana) // <-- color is gone.

const bananaWithColor = FruitSchema.loose().parse(bananaRaw)
console.log(bananaWithColor)

// const bananaShouldNotOverProperty = FruitSchema.strict().parse(bananaRaw) // error!
