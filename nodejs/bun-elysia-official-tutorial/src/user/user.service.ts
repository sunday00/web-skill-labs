import { MySql2Database } from 'drizzle-orm/mysql2'
import { db } from '../configs/db/drizzle.db.config'
import { Static, t } from 'elysia'
import { usersTable } from '../configs/db/migs/schema'

export const userCreateInput = t.Object({
  name: t.String(),
  age: t.Integer(),
  email: t.String(),
})

export type UserCreateInput = Static<typeof userCreateInput>

class UserService {
  constructor(private readonly db: MySql2Database) {}

  public async store(dto: UserCreateInput) {
    return this.db.insert(usersTable).values(dto).$returningId()
  }
}

export const userService = new UserService(db)
