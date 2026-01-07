import { registerAs } from '@nestjs/config'

export const dbConfig = () => ({
  connString: process.env.DB_CONN_URL ?? '',
  dbName: process.env.DB_CONN_NAME ?? '',
})

export const dbConn = registerAs('dbConfig', () => dbConfig())
