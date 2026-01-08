import { Box, Container, Heading, Separator, Stack } from '@chakra-ui/react'
import { Navigate, Route, Routes } from 'react-router'
import Standard from '@/layouts/standard.tsx'

function App() {
  return (
    <Container
      w={'90vw'}
      p={4}
      as={'section'}
      className={'container root-container'}
    >
      <Stack>
        <Heading as={'h1'} fontWeight={'bold'}>
          <a href={import.meta.env.VITE_APP_HOST}>
            React ⚛ Graphql Any Research
          </a>
        </Heading>
        {/*<Nav />*/}
      </Stack>

      <Separator borderColor={'orange'} my={2} />

      <Box as={'section'}>
        <Routes>
          <Route path="/*" element={<Standard />} />

          <Route path="/" element={<Navigate to="/home" replace />} />
        </Routes>
      </Box>
    </Container>
  )
}

export default App
