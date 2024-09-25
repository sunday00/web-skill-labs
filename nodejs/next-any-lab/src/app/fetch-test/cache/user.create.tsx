'use client'

import {
  Button,
  Flex,
  FormControl,
  FormHelperText,
  FormLabel,
  Input,
} from '@chakra-ui/react'
import { ChangeEvent, useState } from 'react'

const UserCreate = () => {
  const [name, setName] = useState('')
  const [age, setAge] = useState(0)

  const handleNameChange = (e: ChangeEvent<HTMLInputElement>) => {
    setName(e.target.value)
  }

  const handleAgeChange = (e: ChangeEvent<HTMLInputElement>) => {
    setAge(Number(e.target.value))
  }

  return (
    <Flex>
      <FormControl>
        <FormLabel htmlFor="name">name</FormLabel>
        <Input
          id="name"
          type="text"
          name="name"
          value={name ?? ''}
          onChange={handleNameChange}
        />
        <FormHelperText>name of user</FormHelperText>
      </FormControl>

      <FormControl>
        <FormLabel htmlFor="age">age</FormLabel>
        <Input
          id="age"
          type="umber"
          name="age"
          value={age ?? 0}
          onChange={handleAgeChange}
        />
        <FormHelperText>age of user</FormHelperText>
      </FormControl>

      <Button type="submit">send</Button>
    </Flex>
  )
}

export { UserCreate }
