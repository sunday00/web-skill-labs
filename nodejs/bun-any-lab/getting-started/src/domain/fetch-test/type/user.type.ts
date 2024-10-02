import { t } from 'elysia'

export const user = t.Object({
  id: t.Number(),
  name: t.String(),
  age: t.Number(),
})

export type UserType = typeof user.static
