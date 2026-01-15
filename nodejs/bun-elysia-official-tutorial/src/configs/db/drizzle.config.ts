import { defineConfig } from 'drizzle-kit'

export default defineConfig({
  out: './drizzle',
  schema: './src/configs/db/migs/schema.ts',
  dialect: 'mysql',
  dbCredentials: {
    host: process.env.DATABASE_HOST!,
    port: Number(process.env.DATABASE_PORT ?? 3306),
    user: process.env.DATABASE_USER,
    password: process.env.DATABASE_PSWD!.replaceAll('\\', ''),
    database: process.env.DATABASE_DBNM!,
  },
})
