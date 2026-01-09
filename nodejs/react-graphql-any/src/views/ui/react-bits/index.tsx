import { Box, For, List, SimpleGrid } from '@chakra-ui/react'
import { Route, Routes } from 'react-router'
import ReactBitsMenu from '@/views/ui/react-bits/menu'

const ReactBits = () => {
  return (
    <Routes>
      <Route path="/" element={<SiteMap />}></Route>
      <Route path="/menu" element={<ReactBitsMenu />}></Route>
    </Routes>
  )
}

const SiteMap = () => {
  const siteMapInThis = [{ id: 0, label: 'menu', href: '/ui/react-bits/menu' }]

  return (
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
  )
}

export default ReactBits
