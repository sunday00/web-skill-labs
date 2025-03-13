import Fieldset from '@/components/form/fieldset'
import { Form, useActionData } from '@remix-run/react'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import { FaEnvelope, FaFly, FaLock, FaUser } from 'react-icons/fa6'
import Button from '@/components/form/button'
import { ActionFunction } from '@remix-run/node'
import { useEffect } from 'react'
import { CommonRes } from '@/common/common.entity'

export const action: ActionFunction = async ({ request }) => {
  const url = `${process.env.API_HOST}/api/v1/auth/register`
  const fd = await request.formData()

  const res = await fetch(url, {
    method: 'POST',
    body: JSON.stringify({
      email: fd.get('email'),
      name: fd.get('name'),
      password: fd.get('password'),
    }),
    headers: {
      'Content-Type': 'application/json; charset=utf-8',
    },
  })

  return res.json()
}

export default function Signup() {
  const actionRes = useActionData<CommonRes<unknown>>()

  useEffect(() => {
    if (actionRes && 'data' in actionRes) {
      console.log('success')
    } else if (actionRes?.errorData) {
      console.error(actionRes?.errorData)
    }
  }, [actionRes, actionRes?.statusCode])

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
                autoComplete={'email'}
                icon={<FaEnvelope />}
                placeholder={'abc@def.com'}
                required={true}
              />
            </div>

            <div className={'form-control form-control-signup form-control-signup-name'}>
              <Input
                label={'name'}
                name={'name'}
                type={'name'}
                autoComplete={'username'}
                icon={<FaUser />}
                placeholder={'doctor_spider'}
                required={true}
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
