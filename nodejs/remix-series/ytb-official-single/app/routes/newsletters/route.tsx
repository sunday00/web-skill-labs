import Title from '@/components/texts/title'
import Fieldset from '@/components/form/fieldset'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import EmailIcon from '@/components/icons/email.icon'
import Button from '@/components/form/button'
import { ActionFunction } from '@remix-run/node'
import { Form, useActionData } from '@remix-run/react'
import { useEffect, useState } from 'react'
import Select from '@/components/form/select'
import Flex from '@/components/layouts/flex'
import { formData } from '@/utils/form.data'
import { useToast } from '@/hooks/useToast'

export const action: ActionFunction = async ({ request }) => {
  const fd = await formData(request)
  const email = fd.get<string>('email')

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
  const [err, _setErr] = useState<string>('')

  // const global = useContext(GlobalContext)
  const { addAlert } = useToast()

  useEffect(() => {}, [afterAction])

  const testToast = () => {
    addAlert({
      status: 'success',
      duration: 5,
      title: 'success!!',
      message: 'successfully subscribe',
    })
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

                <Flex>
                  <Select
                    name={'selectTest'}
                    w={'500px'}
                    options={new Array(60).fill(0).map((a, idx) =>
                      idx % 15
                        ? {
                            show: `item-${idx}`,
                            value: idx,
                          }
                        : {
                            value: 'abrae golsdfewgfdsjhfjkdskd - ' + idx,
                          },
                    )}
                    optionClassName={'text-2xl'}
                    label={'something'}
                    defaultSelect={{
                      show: `item-3`,
                      value: 3,
                    }}
                    errorMessage={'ooooo'}
                  />

                  <Select
                    name={'selectTest2'}
                    options={new Array(4).fill(0).map((a, idx) =>
                      idx % 15
                        ? {
                            show: `item-${idx}`,
                            value: idx,
                          }
                        : {
                            value: 'a - ' + idx,
                          },
                    )}
                  />
                </Flex>

                <Button
                  type={'submit'}
                  text={'Subscribe'}
                  className={'mx-auto'}
                  name={'_action'}
                  value={'submit'}
                  w={'w-full'}
                />
              </Box>
            </Fieldset>
          </Box>
        </Form>

        <button
          className="btn w-full"
          type={'submit'}
          name={'_action'}
          value={'testToast'}
          onClick={testToast}
        >
          open modal
        </button>
      </Box>
    </section>
  )
}
