import { Box, For, List, SimpleGrid } from '@chakra-ui/react'

const Home = () => {
  const siteMapInThis = [{ id: 0, label: 'infra', href: '/infra' }]

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

export default Home
