import { z } from 'zod'

const UserSchema = z.object({
  username: z.string(),
  friends: z.array(z.string()),
  educated: z.array(z.string()).nonempty(),
  coords: z.tuple([z.number(), z.number(), z.number()]),
  flex: z.tuple([z.boolean(), z.string()]).rest(z.number()),
})

const userRaw: z.infer<typeof UserSchema> = {
  username: 'sunday00',
  friends: ['smith', 'michell'],
  educated: ['JumboElementary', 'KobraMiddle', 'TigerHigh'],
  coords: [23, 4, 122],
  flex: [false, 'hello', 1, 2, 3],
}

const user = UserSchema.parse(userRaw)

console.log(user)
console.log(UserSchema.shape.friends.element)
