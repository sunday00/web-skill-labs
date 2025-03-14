import { UserRole } from '@/entities/user.entity'
import { createCookie } from '@remix-run/node'
import { ToastProps } from '@/components/feedbacks/toast'
import { time } from '@/utils/time'

export const COOKIE_SECRET = process?.env.COOKIE_SECRET || ''
if (!COOKIE_SECRET.length) {
  console.warn('🚨 Cookie secret should set fot security.')
}

export type JWTPayload = {
  id: string
  role: UserRole
  name: string
  email: string
  iat: number
  exp: number
}

export function parseJwt(token: string) {
  const base64Url = token.split('.')[1]
  const base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/')
  const json = JSON.parse(Buffer.from(base64, 'base64').toString('utf-8'))

  json['iat'] = json['iat'] * 1000
  json['exp'] = json['exp'] * 1000

  return json as JWTPayload
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

export const getCookie = async (name: string, request: Request) => {
  const cookieString = request.headers.get('Cookie')
  return createCookie(name, { secrets: [COOKIE_SECRET] }).parse(cookieString)
}

export const clearCookie = async (request: Request) => {
  const cookieString = request.headers.get('Cookie')?.split(';') ?? []

  const headers = new Headers()

  await Promise.all(
    cookieString.map(async (c) => {
      const nameValue = c.split('=')

      headers.append(
        'Set-Cookie',
        await createCookie(nameValue[0], { secrets: [COOKIE_SECRET] }).serialize('', {
          expires: new Date(),
          maxAge: 1,
        }),
      )
    }),
  )

  return headers
}

export async function getToasts(request: Request) {
  const exists = await getCookie('toasts', request)
  if (!exists) return [] as ToastProps[]

  return JSON.parse(exists) as ToastProps[]
}

export async function setToast(toast: ToastProps, request: Request) {
  const toasts = await getToasts(request)
  toasts.push(toast)

  return generateCookie('toasts', time().add(1, 'years').unix() * 1000, JSON.stringify(toasts))
}
