import { registerAs } from '@nestjs/config'
import { JwtModuleOptions, JwtSignOptions } from '@nestjs/jwt'

export const jwtConfig = () => ({
  secret: process.env.JWT_SEC ?? '',
  expiresIn: process.env.JWT_EXP || '3h',
  expiresRefreshIn: process.env.JWT_REF || '7d',
})

export const jwt = registerAs('jwt', () => jwtConfig())

export const jwtOptions = (): JwtModuleOptions => ({
  global: true,
  secret: jwtConfig().secret,
  signOptions: { expiresIn: jwtConfig().expiresIn } as JwtSignOptions,
})
