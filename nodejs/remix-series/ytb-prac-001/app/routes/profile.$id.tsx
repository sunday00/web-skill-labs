import { findUser, User } from '../../users'
import { Form, redirect, useLoaderData } from '@remix-run/react'
import { Box } from '~/components/Box'

export const loader = async ({ params }: { params: { id: string } }) => {
  const user = findUser(params.id)

  if (!user) return redirect('/')

  return new Response(JSON.stringify(user), {
    headers: { 'Content-Type': 'application/json' },
    status: 200,
  })
}

export const action = async ({ params, request }: { params: { id: string }; request: Request }) => {
  const formData = await request.formData()
  const action = formData.get('action')

  return redirect('/')
}

const Profile = () => {
  const user = useLoaderData<User>()

  const handleLClientLogout = () => {
    localStorage.removeItem('userLogged')
  }

  return (
    <Box>
      <h1 className={'text-2xl font-bold'}>Profile</h1>

      <Box>
        <div>{user.name}</div>
        <div>{user.email}</div>
      </Box>

      <div>
        <Form method={'POST'} onSubmit={handleLClientLogout}>
          <input type="hidden" name={'action'} value={'logout'} />
          <button type={'submit'} className={'btn btn-sm btn-outline btn-error'}>
            logout
          </button>
        </Form>
      </div>
    </Box>
  )
}

export default Profile
