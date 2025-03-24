import Box from '@/components/layouts/box'
import Flex from '@/components/layouts/flex'
import SnsGoogle from '@/routes/auth/sns/components/sns.google'
import SnsKakao from '@/routes/auth/sns/components/sns.kakao'
import SnsNaver from '@/routes/auth/sns/components/sns.naver'
import { Outlet } from '@remix-run/react'

export default function AuthSns() {
  return (
    <Box>
      <Flex className={'justify-start gap-2'}>
        <SnsGoogle />
        <SnsKakao />
        <SnsNaver />
        <Outlet />
      </Flex>
    </Box>
  )
}
