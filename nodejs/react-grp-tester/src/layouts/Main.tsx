import { Route, Routes } from 'react-router'
import Home from '@/views/home'
import { Box } from '@chakra-ui/react'
import { Proam } from '@/views/proam/proam.index.tsx'

const MainLayout = () => {
  return (
    <Box w="full" mt={4}>
      <Routes>
        <Route path="/home" element={<Home />}></Route>
        <Route path="/proam/*" element={<Proam />}></Route>
      </Routes>
    </Box>
  )
}

export default MainLayout
