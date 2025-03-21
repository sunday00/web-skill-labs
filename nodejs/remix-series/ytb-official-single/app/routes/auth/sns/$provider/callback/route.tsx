import { LoaderFunction } from '@remix-run/node'
import { generateCookie } from '@/routes/auth/signin/cookie.manager'
import { time } from '@/utils/time'
import { redirect } from '@remix-run/react'

export const loader: LoaderFunction = async ({ request, params }) => {
  const currentUrl = new URL(request.url)
  const code = currentUrl.searchParams.get('code')

  const url = `${process.env.API_HOST}/api/v1/auth/sns`

  const res = await fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      provider: params.provider,
      code: code,
    }),
  })

  const raw = await res.json()

  if ('errorData' in raw) {
    return raw
  }

  const headers = new Headers()
  headers.append(
    'Set-Cookie',
    await generateCookie(
      'access-token',
      time().add(1, 'weeks').unix() * 1000,
      raw.data.accessToken,
    ),
  )

  return redirect('/', {
    headers,
  })
}

export default function AuthSnsCallback() {
  return <></>
}
