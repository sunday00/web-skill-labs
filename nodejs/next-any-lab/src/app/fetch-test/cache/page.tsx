import { User } from './type'
import {UserList} from './user.list'

const fetchUsers = async (page = 1) => {
  const raw = await fetch('http://localhost:3400/fetchTest?page=' + page, {
    next: { tags: ['fetchTest'] },

    method: 'GET',
  })

  return raw.json()
}

const FetchTestPage = async ({
  searchParams,
}: {
  searchParams: { page: string }
}) => {
  const users = await fetchUsers(Number(searchParams.page ?? 1))

  return (
    <>
      <UserList users={users as unknown as User[]} />
    </>
  )
}

export default FetchTestPage
