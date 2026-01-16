import { MySql2Database } from 'drizzle-orm/mysql2'
import { db } from '../configs/db/drizzle.db.config'
import { Static, t } from 'elysia'
import { usersTable } from '../configs/db/migs/schema'
import { desc } from 'drizzle-orm'

export const userCreateInput = t.Object({
  name: t.String(),
  age: t.Integer(),
  email: t.String(),
})

export type UserCreateInput = Static<typeof userCreateInput>

export const paginateInput = t.Object({
  page: t.Integer(),
  size: t.Integer(),
})

export type PaginateInput = Static<typeof paginateInput>

class UserService {
  constructor(private readonly db: MySql2Database) {}

  public async store(dto: UserCreateInput) {
    return this.db.insert(usersTable).values(dto).$returningId()
  }

  public async findManyWithCount(dto: PaginateInput) {
    return Promise.all([
      this.db
        .select({ id: usersTable.id, name: usersTable.name })
        .from(usersTable)
        .orderBy(desc(usersTable.id))
        .offset((dto.page - 1) * dto.size)
        .limit(dto.size),
      this.db.$count(usersTable),
    ])
  }
}

export const userService = new UserService(db)
