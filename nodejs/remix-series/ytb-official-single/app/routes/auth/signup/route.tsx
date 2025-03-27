import Fieldset from '@/components/form/fieldset'
import { Form, useActionData } from '@remix-run/react'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import { FaEnvelope, FaFly, FaLock, FaUser } from 'react-icons/fa6'
import Button from '@/components/form/button'
import { ActionFunction } from '@remix-run/node'
import { CommonRes } from '@/common/common.entity'
import { formData } from '@/utils/form.data'
import { useFormError } from '@/hooks/error.message'
import { useEffect } from 'react'

export const action: ActionFunction = async ({ request }) => {
  const url = `${process.env.API_HOST}/api/v1/auth/register`
  const fd = await formData(request)

  const payload = {
    email: fd.get<string>('email'),
    name: fd.get<string>('name'),
    password: fd.get<string>('password'),
  }

  if (payload.password.length < 4) {
    return {
      errorData: {
        error: 'tooShortPassword',
        message: 'Password must be longer than 4 characters',
      },
    }
  }

  const res = await fetch(url, {
    method: 'POST',
    body: JSON.stringify(payload),
    headers: {
      'Content-Type': 'application/json; charset=utf-8',
    },
  })

  return res.json()
}

export default function Signup() {
  const actionRes = useActionData<CommonRes<unknown>>()
  const [formError, setFormError] = useFormError({ email: '', password: '', name: '' })

  useEffect(() => {
    if (actionRes && 'data' in actionRes) {
      console.log('success')
    } else if (actionRes?.errorData) {
      setFormError(actionRes.errorData.error)
    }
  }, [actionRes, setFormError])

  return (
    <section>
      <Fieldset legend={'signup'}>
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

            <div className={'form-control form-control-signup form-control-signup-name'}>
              <Input
                label={'name'}
                name={'name'}
                type={'name'}
                autoComplete={'name'}
                icon={<FaUser />}
                placeholder={'doctor_spider'}
                required={true}
                errorMessage={formError.name}
              />
            </div>

            <div className={'form-control form-control-signup form-control-signup-password'}>
              <Input
                label={'password'}
                name={'password'}
                type={'password'}
                autoComplete={'new-password'}
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
