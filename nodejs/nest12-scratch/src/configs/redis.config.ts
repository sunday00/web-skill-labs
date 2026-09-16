import { registerAs } from '@nestjs/config'

export default registerAs('RedisConfig', () => ({
  host: 'localhost',
  port: 6380,
  password: 'kOmedy',
  db: 5,
}))
