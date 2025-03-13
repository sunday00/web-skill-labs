import { UserRole } from '@/entities/user.entity'
import { createCookie } from '@remix-run/node'

export const COOKIE_SECRET = process.env.COOKIE_SECRET || ''
if (!COOKIE_SECRET.length) {
  console.warn('🚨 Cookie secret should set fot security.')
}

export function parseJwt(token: string) {
  const base64Url = token.split('.')[1]
  const base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/')
  const json = JSON.parse(Buffer.from(base64, 'base64').toString('utf-8'))

  json['iat'] = json['iat'] * 1000
  json['exp'] = json['exp'] * 1000

  return json as {
    id: string
    role: UserRole
    name: string
    email: string
    iat: number
    exp: number
  }
}

export const generateCookie = async (name: string, exp: number, value: string) => {
  const cookie = createCookie(name, {
    httpOnly: true,
    path: '/',
    sameSite: 'lax',
    secrets: [COOKIE_SECRET],
    secure: process.env.NODE_ENV === 'production',
  })

  return cookie.serialize(value, { expires: new Date(exp) })
}
