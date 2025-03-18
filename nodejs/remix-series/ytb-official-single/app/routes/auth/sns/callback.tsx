import { LoaderFunction } from '@remix-run/node'

export const loader: LoaderFunction = async ({ request }) => {
  const currentUrl = new URL(request.url)
  const code = currentUrl.searchParams.get('code')

  const url = `${process.env.API_HOST}/api/v1/auth/sns`

  const res = await fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      provider: 'google',
      code: code,
    }),
  })

  // TODO
  console.log(await res.json())
  // store cookie with our-app-accessToken
  // redirect to home

  console.warn('NOT IMPLEMENTED YET')
  return null
}

export default function AuthSnsCallback() {
  return <></>
}
