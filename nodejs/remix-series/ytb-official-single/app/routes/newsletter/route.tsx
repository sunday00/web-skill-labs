import Title from '@/components/texts/title'
import Fieldset from '@/components/form/fieldset'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import EmailIcon from '@/components/icons/email.icon'
import Button from '@/components/form/button'
import { ActionFunction } from '@remix-run/node'
import { Form } from '@remix-run/react'

export const action: ActionFunction = async ({ request }) => {
  const fd = await request.formData()
  const email = fd.get('email')

  console.log(email)

  return { statusCode: 200 }
}

export default function Newsletter() {
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
                />

                <Button type={'submit'} text={'Subscribe'} className={'mx-auto'} />
              </Box>
            </Fieldset>
          </Box>
        </Form>
      </Box>
    </section>
  )
}
