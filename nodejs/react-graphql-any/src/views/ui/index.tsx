import { Box, For, List, SimpleGrid } from '@chakra-ui/react'
import { Route, Routes } from 'react-router'
import ReactBits from '@/views/ui/react-bits'

const Ui = () => {
  return (
    <Routes>
      <Route path="/" element={<SiteMap />}></Route>
      <Route path="/react-bits/*" element={<ReactBits />}></Route>
    </Routes>
  )
}

const SiteMap = () => {
  const siteMapInThis = [{ id: 0, label: 'react-bits', href: '/ui/react-bits' }]

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

export default Ui
