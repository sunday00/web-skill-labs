import { registerAs } from '@nestjs/config'

export default registerAs('env', () => ({
  port: Number(process.env.APP_PORT ?? 3000),
}))
