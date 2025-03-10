import Title from '@/components/texts/title'
import Fieldset from '@/components/form/fieldset'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import EmailIcon from '@/components/icons/email.icon'
import Button from '@/components/form/button'
import { ActionFunction } from '@remix-run/node'
import { Form, useActionData } from '@remix-run/react'
import { useContext, useEffect, useState } from 'react'
import { GlobalContext } from '@/providers/global.context.provider'
import Select from '@/components/form/select'

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

  const global = useContext(GlobalContext)

  useEffect(() => {
    if (afterAction?.[1].error) setErr(afterAction?.[1]?.message ?? '')
    else setErr('')

    if (afterAction?.[0] === 200) {
      global.toasts.push({
        status: 'success',
        duration: 5,
        title: 'success!!',
        message: 'successfully subscribe. check email box',
        key: global.toasts.length + new Date().getTime() + Math.random().toString(),
      })

      global.update(global)

      afterAction[0] = 0
    }
  }, [afterAction, global])

  const testToast = () => {
    global.toasts.push({
      status: 'success',
      duration: 5,
      title: 'success!!',
      message: 'successfully subscribe',
      key: global.toasts.length + new Date().getTime() + Math.random().toString(),
    })
    global.update(global)
  }

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

                <Select
                  name={'selectTest'}
                  options={new Array(4).fill(0).map((a, idx) =>
                    idx % 15
                      ? {
                          show: `item-${idx}`,
                          value: idx,
                        }
                      : { value: 'abracadabra abolute gold - ' + idx },
                  )}
                />

                <Button type={'submit'} text={'Subscribe'} className={'mx-auto'} />
              </Box>
            </Fieldset>
          </Box>
        </Form>

        <button className="btn" onClick={testToast}>
          open modal
        </button>
      </Box>
    </section>
  )
}
