import { Flex, Group } from '@chakra-ui/react'

const Nav = () => {
  return (
    <Flex justify={'space-between'}>
      <Group>
        <a href="/proam">proam</a>
        {/*<Separator orientation="vertical" height="3" borderColor={'gray.400'} />*/}
      </Group>
    </Flex>
  )
}

export default Nav
