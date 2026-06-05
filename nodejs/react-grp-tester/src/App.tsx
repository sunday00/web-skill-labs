import { Navigate, Route, Routes } from 'react-router'
import { Box, Container, Heading, Separator, Stack } from '@chakra-ui/react'
import Main from '@/layouts/Main.tsx'
import Nav from '@/components/nav'

function App() {
  return (
    <Container w={'90vw'} p={4} as={'section'}>
      <Stack>
        <Heading as={'h1'} fontWeight={'bold'} className={'ob-main-title'}>
          <a href={import.meta.env.VITE_APP_HOST}>react-grp-tester</a>
        </Heading>
        <Nav />
      </Stack>

      <Separator borderColor={'orange'} my={2} />

      <Box as={'section'}>
        <Routes>
          <Route path="/*" element={<Main />} />

          <Route path="/" element={<Navigate to="/home" replace />} />
        </Routes>
      </Box>
    </Container>
  )
}

export default App
