'use server'


import { User } from './type'
import {handleCreateUser} from "@/app/fetch-test/cache/actions";
import {UserCreate} from "@/app/fetch-test/cache/user.create";
import {Flex, Stack, theme} from "@chakra-ui/react";


const UserList = async ({ users = [] }: { users: User[] }) => {


  const list =
    users?.map((user) => {
      return (
        <Flex key={user.id!} gap={theme.space['4']}>
          <p>{user.id}</p>
          <p>{user.name}</p>
          <p>{user.age}</p>
        </Flex>
      )
    }) ?? []

  return (
    <section>
      <form action={handleCreateUser}>
        <UserCreate />
      </form>
      <Stack>{list}</Stack>
    </section>
  )
}

export {UserList}
