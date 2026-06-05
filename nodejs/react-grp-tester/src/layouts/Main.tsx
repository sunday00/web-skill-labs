import { Route, Routes } from 'react-router'
import Home from '@/views/home'
import { Box } from '@chakra-ui/react'
import Game from '@/views/game'

const MainLayout = () => {
  return (
    <Box w="full" mt={4}>
      <Routes>
        <Route path="/home" element={<Home />}></Route>
        <Route path="/game/*" element={<Game />}></Route>
      </Routes>
    </Box>
  )
}

export default MainLayout
