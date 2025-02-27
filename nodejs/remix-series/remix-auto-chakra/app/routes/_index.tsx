import type { MetaFunction } from '@remix-run/node'
import { Box, Button, HStack } from '@chakra-ui/react'
import { useColorMode } from '~/components/ui/color-mode'

export const meta: MetaFunction = () => {
  return [{ title: 'New Remix App' }, { name: 'description', content: 'Welcome to Remix!' }]
}

export default function Index() {
  const { toggleColorMode } = useColorMode()

  const handle = () => {
    console.log('fds')
    toggleColorMode()
  }

  return (
    <Box>
      <style>
        {`
        h1 {
         color: red;
        }
      `}
      </style>

      <h1>HI</h1>

      <HStack>
        <Button colorPalette={'blue'} variant="surface">
          Click me
        </Button>
        <Button onClick={handle}>Click me</Button>
      </HStack>
    </Box>
  )
}
