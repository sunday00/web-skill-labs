import { api } from '@/api/API.ts'

const apiUrl = import.meta.env.VITE_APP_API

export const authLoginByKakaoCode = async (code: string) => {
  return (await api(`${apiUrl}/api/rest/auth/sign`, {
    method: 'POST',
    body: JSON.stringify({ code }),
  })) as [boolean, { token: string }]
}

export const authMe = async () => {
  const accessToken = localStorage.getItem('accessToken')
  if (!accessToken) return [false, null]

  return await api(`${apiUrl}/api/rest/auth/me`, {
    method: 'GET',
  })
}
