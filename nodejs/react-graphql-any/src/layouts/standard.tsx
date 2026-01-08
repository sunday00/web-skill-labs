import { Route, Routes } from 'react-router'
import Home from '@/views/home'
import { Box } from '@chakra-ui/react'
import Infra from '@/views/infra'

const Standard = () => {
  return (
    <Box w="full" mt={4} className={'layout standard-layout'}>
      <Routes>
        <Route path="/home/*" element={<Home />}></Route>
        <Route path="/infra/*" element={<Infra />}></Route>
      </Routes>
    </Box>
  )
}

export default Standard
