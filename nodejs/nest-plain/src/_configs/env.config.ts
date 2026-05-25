import { registerAs } from '@nestjs/config'
import { config } from 'dotenv'
import path from 'node:path'

config({ path: path.join(process.cwd(), `.env.${process.env.NODE_ENV}`) })

export const ENV: {
  [K: Uppercase<string>]: string | undefined
  get: <T>(k: string, defaultValue?: T) => T | string
} = {
  ...process.env,
  get<T>(k: string, defaultValue?: T) {
    const v = this[k]

    if (v) return v

    if (defaultValue) return defaultValue

    return ''
  },
}

export const envs = registerAs('env', () => ENV)
