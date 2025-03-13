import Fieldset from '@/components/form/fieldset'
import { Form, redirect, useActionData } from '@remix-run/react'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import { FaEnvelope, FaFly, FaLock } from 'react-icons/fa6'
import Button from '@/components/form/button'
import { ActionFunction } from '@remix-run/node'
import { CommonRes } from '@/common/common.entity'
import { formData } from '@/utils/form.data'
import { useFormError } from '@/hooks/error.message'
import { useEffect } from 'react'
import { generateCookie, parseJwt } from '@/routes/auth/signin/cookie.manager'

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

  const resPayload = parseJwt(raw.data.accessToken)
  const headers = new Headers()
  headers.append(
    'Set-Cookie',
    await generateCookie('access-token', resPayload.exp, raw.data.accessToken),
  )
  headers.append(
    'Set-Cookie',
    await generateCookie('now', new Date().getTime() + 1000, new Date().toString()),
  )
  headers.append('Set-Cookie', await generateCookie('another', resPayload.exp, 'this is another'))

  return redirect('/articles', {
    headers,
  })
}

export default function SignIn() {
  const actionRes = useActionData<CommonRes<unknown>>()
  const [formError, setFormError] = useFormError({ email: '', password: '' })

  useEffect(() => {
    if (actionRes && 'data' in actionRes) {
      console.log('success')
    } else if (actionRes?.errorData) {
      setFormError(actionRes.errorData.error)
    }
  }, [actionRes, setFormError])

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
