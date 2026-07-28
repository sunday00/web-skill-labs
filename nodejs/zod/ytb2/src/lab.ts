import { z } from 'zod'

export const userDataSchema = z.object({
  id: z.templateLiteral([z.number().int().nonnegative()]).or(z.coerce.number()), // number or numeric
  age: z.templateLiteral([z.number().int().nonnegative()]).or(z.coerce.number()), // number or numeric
})

type UserData = z.infer<typeof userDataSchema>

export function processUserData(userData: UserData): string {
  const result = userDataSchema.safeParse(userData)

  if (!result.success) console.warn(result.error)
  const userSummary = `
    User: (ID: ${userData.id}) 
  `

  return userSummary.trim()
}

const userDataFromApi: UserData = {
  id: 22,
  age: '12',
}

const r = processUserData(userDataFromApi)

console.log(r)
