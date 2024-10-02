import { Elysia, t } from 'elysia'
import { User } from './decorator/user.class'
import { UserType } from './type/user.type'

const fetchTest = new Elysia({ prefix: '/fetchTest' })
  .decorate('user', new User())
  .get('/', async ({ user, query: { page } }) => {
    await user.index(page ?? '1')

    return user.data
  })
  .post('/', ({ user, body }: { user: User; body: string }) => {
    const values = JSON.parse(body)
    return user.store(values as unknown as Omit<UserType, 'id'>)
  })

export { fetchTest }
