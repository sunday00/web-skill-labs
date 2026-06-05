import { Navigate, Route, Routes } from 'react-router'
import { Box } from '@chakra-ui/react'
import Login from '@/views/auth/login'
import Callback from '@/views/auth/login/callback.tsx'
import Logout from '@/views/auth/logout'

const AuthLayout = () => {
  return (
    <Box w="full" mt={4}>
      <Routes>
        <Route path="login" element={<Login />}></Route>
        <Route path="callback" element={<Callback />}></Route>

        <Route path="logout" element={<Logout />}></Route>

        <Route path="/" element={<Navigate to="/auth/login" replace />} />
      </Routes>
    </Box>
  )
}

export default AuthLayout
