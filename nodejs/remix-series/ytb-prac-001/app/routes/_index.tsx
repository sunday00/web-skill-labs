import type { MetaFunction } from '@remix-run/node'
import { Box } from '~/components/Box'
import { Flex } from '~/components/Flex'
import { Form, useActionData, useNavigate } from '@remix-run/react'
import { addUser, findUserByEmailPassword, User } from '../../users'
import { nanoid } from 'nanoid'
import { useEffect } from 'react'

export const meta: MetaFunction = () => {
  return [{ title: 'New Remix App' }, { name: 'description', content: 'Welcome to Remix!' }]
}

type ActionData = {
  error?: string
  user?: User
}

export const action = async ({ request }: { request: Request }) => {
  const formData = await request.formData()
  const submittedUser: User = {
    id: '',
    name: formData.get('name') as string,
    email: formData.get('email') as string,
    password: formData.get('password') as string,
  }

  if (!submittedUser.email || !submittedUser.password) {
    return Response.json({ error: 'email, pw required' }, { status: 400 })
  }

  const existing = findUserByEmailPassword(submittedUser.email, submittedUser.password)

  let user: User
  if (existing) user = existing
  else {
    submittedUser.id = nanoid()

    addUser(submittedUser)

    user = submittedUser
  }

  return Response.json({ user }, { status: 200 })
}

export default function Index() {
  const actionData = useActionData<ActionData>()

  const navigate = useNavigate()

  useEffect(() => {
    const storedUser = localStorage.getItem('userLogged')
    if (storedUser) {
      const user = JSON.parse(storedUser) as User
      navigate(`/profile/${user.id}`)
      return
    }

    if (actionData?.user) {
      localStorage.setItem('userLogged', JSON.stringify(actionData.user))
      navigate(`/profile/${actionData.user.id}`)
    }
  }, [actionData, navigate])

  return (
    <div className="container">
      <Flex h={'dvh'} w={'dvw'} justify={'center'} align={'center'}>
        <Box>
          <h2 className={'text-xl text-center font-bold'}>Login</h2>
          <Form method={'POST'}>
            <Box gap={4}>
              <div className={'form-control w-full max-w-xs'}>
                <label htmlFor="name" className={'label label-text'}>
                  name
                </label>
                <input
                  type="text"
                  name={'name'}
                  placeholder="Type here"
                  className="input input-bordered w-full max-w-xs"
                  id={'name'}
                  autoComplete={'name'}
                />
              </div>

              <div className={'form-control w-full max-w-xs'}>
                <label htmlFor="email" className={'label label-text'}>
                  email
                </label>
                <input
                  type="email"
                  name={'email'}
                  placeholder="Type here"
                  className="input input-bordered w-full max-w-xs"
                  id={'email'}
                  autoComplete={'email'}
                />
              </div>

              <div className={'form-control w-full max-w-xs'}>
                <label htmlFor="password" className={'label label-text'}>
                  password
                </label>
                <input
                  type="password"
                  name={'password'}
                  placeholder="Type here"
                  className="input input-bordered w-full max-w-xs"
                  id={'password'}
                  autoComplete={'new-password'}
                />
              </div>

              <div className={'mt-4 flex justify-center'}>
                <button className="btn btn-primary" type={'submit'}>
                  submit
                </button>
              </div>
            </Box>
          </Form>
        </Box>
      </Flex>
    </div>
  )
}
