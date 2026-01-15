import {
  int,
  mysqlTable,
  serial,
  timestamp,
  varchar,
} from 'drizzle-orm/mysql-core'

export const usersTable = mysqlTable('users', {
  id: serial().primaryKey(),
  name: varchar({ length: 255 }).notNull(),
  age: int().notNull(),
  // gender: int(),
  email: varchar({ length: 255 }).notNull().unique(),
  createdAt: timestamp({ fsp: 3 }).defaultNow(),
})
