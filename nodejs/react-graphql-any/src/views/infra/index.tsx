import { Box, For, List, SimpleGrid } from '@chakra-ui/react'
import { Route, Routes } from 'react-router'
import UFireBase from '@/views/infra/u-firebase'

const Infra = () => {
  return (
    <Routes>
      <Route path="/" element={<SiteMap />}></Route>
      <Route path="/firebase/*" element={<UFireBase />}></Route>
    </Routes>
  )
}

const SiteMap = () => {
  const siteMapInThis = [{ id: 0, label: 'firebase', href: '/infra/firebase' }]

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

export default Infra
