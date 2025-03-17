import { ActionFunction } from '@remix-run/node'
import { clearCookie, getCookie } from '@/routes/auth/signin/cookie.manager'
import { redirect } from '@remix-run/react'
import { MethodNotAllowed } from '@/common/excetions'

export const action: ActionFunction = async ({ request }) => {
  const url = `${process.env.API_HOST}/api/v1/auth/logout`
  const accessToken = await getCookie('access-token', request)

  await fetch(url, {
    method: 'DELETE',
    headers: {
      Accept: 'application/json',
      Authorization: 'Bearer ' + accessToken,
    },
  })

  return redirect('/', {
    headers: await clearCookie(request),
  })
}

export default function SignOut() {
  return <MethodNotAllowed />
}
