import { z } from 'zod'

const UserSchema = z.object({
  id: z.union([z.string(), z.number()]),
})

const userRaw1: z.infer<typeof UserSchema> = {
  id: '11111-222-some-uuid-ssss',
}

const userRaw2: z.infer<typeof UserSchema> = {
  id: 1,
}

const user1 = UserSchema.parse(userRaw1)
const user2 = UserSchema.parse(userRaw2)

console.log(user1, user2)

// ---------- same

const UserSchema2 = z.object({
  id: z.string().or(z.number()),
})

const userRaw21: z.infer<typeof UserSchema2> = {
  id: '11111-222-some-uuid-ssss',
}

const userRaw22: z.infer<typeof UserSchema2> = {
  id: 1,
}

const user21 = UserSchema.parse(userRaw21)
const user22 = UserSchema.parse(userRaw22)

console.log(user21, user22)

// --------------- discriminationUnion ----------

const Animal = z.object({
  action: z.discriminatedUnion('title', [
    z.object({ title: z.literal('horse'), run: z.number() }),
    z.object({ title: z.literal('bird'), fly: z.number() }),
  ]),
  status: z.discriminatedUnion('status', [
    z.object({ status: z.literal('success'), data: z.string() }),
    z.object({ status: z.literal('failed'), error: z.instanceof(Error) }),
  ]),
})

const animalRaw: z.infer<typeof Animal> = {
  action: {
    title: 'horse',
    run: 1,
  },
  status: {
    status: 'success',
    data: 'hello Juk-To',
  },
}

Animal.parse(animalRaw)

// ----------- dynamic key ---------- ----------- -----------

const UserMap = z.record(z.string(), z.string())
const users: z.infer<typeof UserMap> = {
  james: 'math',
  july: 'science',
  mickey: 'draw',
}

const UserMap2 = z.map(z.string(), z.string())
const users2: z.infer<typeof UserMap2> = new Map<string, string>([
  ['james', 'math'],
  ['july', 'science'],
  ['mickey', 'draw'],
])
console.log(UserMap2.parse(users2))

const UserSet = z.set(z.string())
const users3: z.infer<typeof UserSet> = new Set<string>([
  'james',
  'james',
  'july',
  'july',
  'july',
  'mickey',
])
console.log(UserSet.parse(users3))
