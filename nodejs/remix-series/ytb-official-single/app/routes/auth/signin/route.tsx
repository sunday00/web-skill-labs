import Fieldset from '@/components/form/fieldset'
import { Form, redirect, useActionData, useLoaderData, useNavigate } from '@remix-run/react'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import { FaEnvelope, FaFly, FaLock } from 'react-icons/fa6'
import Button from '@/components/form/button'
import { ActionFunction, LoaderFunction } from '@remix-run/node'
import { CommonRes } from '@/common/common.entity'
import { formData } from '@/utils/form.data'
import { useFormError } from '@/hooks/error.message'
import { useCallback, useEffect, useState } from 'react'
import { generateCookie, getCookie } from '@/routes/auth/signin/cookie.manager'
import { time } from '@/utils/time'
import { useToast } from '@/hooks/useToast'

export const loader: LoaderFunction = async ({ request }) => {
  const credential = await getCookie('access-token', request)
  if (credential) {
    return { statusCode: 200, title: 'alreadyLogged' }
  }

  return {}
}

export const action: ActionFunction = async ({ request }) => {
  const url = `${process.env.API_HOST}/api/v1/auth/login`
  const fd = await formData(request)

  const payload = {
    email: fd.get<string>('email'),
    password: fd.get<string>('password'),
  }

  const res = await fetch(url, {
    method: 'POST',
    body: JSON.stringify(payload),
    headers: {
      'Content-Type': 'application/json; charset=utf-8',
    },
  })

  const raw = await res.json()

  if ('errorData' in raw) {
    return raw
  }

  // const resPayload = parseJwt(raw.data.accessToken)
  const headers = new Headers()
  headers.append(
    'Set-Cookie',
    await generateCookie(
      'access-token',
      // resPayload.exp,
      time().add(1, 'weeks').unix() * 1000,
      raw.data.accessToken,
    ),
  )

  // test
  headers.append(
    'Set-Cookie',
    await generateCookie('now', new Date().getTime() + 1000, new Date().toString()),
  )

  // test
  headers.append(
    'Set-Cookie',
    await generateCookie('another', time().add(1, 'weeks').unix(), 'this is another'),
  )

  return redirect('/', {
    headers,
  })
}

export default function SignIn() {
  const [loaded, setLoaded] = useState<boolean>(false)
  const { addAlert } = useToast()
  const navigate = useNavigate()

  const loadData = useLoaderData<{ title?: string }>()
  const actionRes = useActionData<CommonRes<unknown>>()
  const [formError, setFormError] = useFormError({ email: '', password: '' })

  const load = useCallback(() => {
    if (!loaded && loadData) {
      setLoaded(true)

      if (loadData.title === 'alreadyLogged') {
        addAlert({ title: 'alreadyLogged', status: 'warning', duration: 5 })
        navigate('/', { replace: true })

        return
      }
    }
  }, [addAlert, loadData, loaded, navigate])

  useEffect(() => {
    load()

    if (actionRes && 'data' in actionRes) {
      console.log('success')
    } else if (actionRes?.errorData) {
      setFormError(actionRes.errorData.error)
    }
  }, [actionRes, load, setFormError])

  return (
    <section>
      <Fieldset legend={'sign in'}>
        <Form method={'post'}>
          <Box>
            <div className={'form-control form-control-signup form-control-signup-email'}>
              <Input
                label={'email'}
                name={'email'}
                type={'email'}
                autoComplete={'username'}
                icon={<FaEnvelope />}
                placeholder={'abc@def.com'}
                required={true}
                errorMessage={formError.email}
              />
            </div>

            <div className={'form-control form-control-signup form-control-signup-password'}>
              <Input
                label={'password'}
                name={'password'}
                type={'password'}
                autoComplete={'current-password'}
                icon={<FaLock />}
                placeholder={'* * * * * *'}
                required={true}
                errorMessage={formError.password}
              />
            </div>

            <div className={'form-control form-control-signup form-control-signup-submitter'}>
              <Button type={'submit'} className={'flex'} text={'Submit'} w={'full'}>
                <FaFly />
              </Button>
            </div>
          </Box>
        </Form>
      </Fieldset>
    </section>
  )
}
