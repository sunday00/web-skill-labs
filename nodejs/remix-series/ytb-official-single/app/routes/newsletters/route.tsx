import Title from '@/components/texts/title'
import Fieldset from '@/components/form/fieldset'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import EmailIcon from '@/components/icons/email.icon'
import Button from '@/components/form/button'
import { ActionFunction } from '@remix-run/node'
import { Form, useActionData } from '@remix-run/react'
import { useEffect, useState } from 'react'
import Toast from '@/components/feedbacks/toast'

export const action: ActionFunction = async ({ request }) => {
  const fd = await request.formData()
  const email = fd.get('email')

  const key = process.env.KIT_KEY
  const url = process.env.KIT_URL
  const fid = process.env.KIT_FID

  const res = await fetch(`${url}/forms/${fid}/subscribe`, {
    method: 'POST',
    body: JSON.stringify({ email, api_key: key }),
    headers: {
      'Content-Type': 'application/json; charset=utf-8',
    },
  })

  return [res.status, await res.json()]
}

export default function Newsletters() {
  const afterAction = useActionData<[number, { error?: string; message?: string }]>()
  const [err, setErr] = useState<string>('')

  useEffect(() => {
    if (afterAction?.[1].error) setErr(afterAction?.[1]?.message ?? '')
    else setErr('')
  }, [afterAction])

  return (
    <section className={''}>
      <Box>
        <Title as={1} text={'Newsletter'} />

        <Form method={'POST'}>
          <Box>
            <Title as={2} text={'Subscribe!'} />

            <Fieldset legend={'send email address'}>
              <Box>
                <Input
                  icon={<EmailIcon />}
                  label={''}
                  type={'email'}
                  name={'email'}
                  autoComplete={'email'}
                  placeholder={'aaa@bbb.ccc'}
                  description={'include @ and .XXX'}
                  errorMessage={err}
                />

                <Button type={'submit'} text={'Subscribe'} className={'mx-auto'} />
              </Box>
            </Fieldset>
          </Box>
        </Form>

        <Toast />

        <button className="btn" onClick={() => document.getElementById('my_modal_3').showModal()}>
          open modal
        </button>
      </Box>
    </section>
  )
}
