import { Box, For, List, SimpleGrid, Stack } from '@chakra-ui/react'
import { Route, Routes } from 'react-router'
import BackendConn from '@/views/home/BackendConn.tsx'

const Home = () => {
  const siteMapInThis = [
    { id: 0, label: 'backendConn', href: '/home/backend-conn' },
    { id: 1, label: 'infra', href: '/infra' },
    { id: 2, label: 'ui', href: '/ui' },
  ]

  return (
    <Stack gap={4}>
      <Routes>
        <Route path="/backend-conn" element={<BackendConn />}></Route>
      </Routes>

      <Box className={'wrap-module-box'}>
        <SimpleGrid columns={3}>
          <List.Root>
            <For each={siteMapInThis}>
              {(item) => {
                return (
                  <List.Item key={item.id}>
                    <a href={item.href}>{item.label}</a>
                  </List.Item>
                )
              }}
            </For>
          </List.Root>
        </SimpleGrid>
      </Box>
    </Stack>
  )
}

export default Home
