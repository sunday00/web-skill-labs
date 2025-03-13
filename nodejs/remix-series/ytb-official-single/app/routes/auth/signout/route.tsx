import { ActionFunction } from '@remix-run/node'
import { clearCookie } from '@/routes/auth/signin/cookie.manager'
import { redirect } from '@remix-run/react'
import { MethodNotAllowed } from '@/common/excetions'

export const action: ActionFunction = async ({ request }) => {
  return redirect('/', {
    headers: await clearCookie(request),
  })
}

export default function SignOut() {
  return <MethodNotAllowed />
}
