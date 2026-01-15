import './migs/schema'

import { $ } from 'bun'
import mysql from 'mysql2/promise'
import { drizzle } from 'drizzle-orm/mysql2'

const pools = mysql.createPool({
  connectionLimit: 4,
  host: process.env.DATABASE_HOST,
  port: Number(process.env.DATABASE_PORT),
  user: process.env.DATABASE_USER,
  password: process.env.DATABASE_PSWD,
  database: process.env.DATABASE_DBNM,
})

const db = drizzle({
  client: pools,
  // logger: true
})

const migrateDB = async () => {
  // const r =
  await $`bun drizzle-kit push --config ./src/configs/db/drizzle.config.ts`
  // console.log(r.text())
}

export { db, migrateDB }
