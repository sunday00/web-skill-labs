import { LoaderFunction } from '@remix-run/node'
import { generateCookie } from '@/routes/auth/signin/cookie.manager'
import { time } from '@/utils/time'
import { redirect, useLoaderData, useNavigate } from '@remix-run/react'
import { useEffect, useState } from 'react'
import { useToast } from '@/hooks/useToast'

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

  if (!raw.data.di) {
    // TODO
    // THIS IS INSANE
    // once store cookie first/
    console.warn({ error: 'missingPhoneVerified' })
    return { error: 'missingPhoneVerified' }
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
  const [loaded, setLoaded] = useState(false)
  const { addAlert } = useToast()
  const navigate = useNavigate()

  const load = useLoaderData<{ error: string }>()

  useEffect(() => {
    if (!loaded && load) {
      setLoaded(true)

      if (load.error === 'missingPhoneVerified') {
        // TODO: real redirect to verifiable page.
        addAlert({ status: 'warning', title: 'NeedPhoneVerify' })

        navigate('/')
      }
    }
  }, [addAlert, load, loaded, navigate])

  return <></>
}
