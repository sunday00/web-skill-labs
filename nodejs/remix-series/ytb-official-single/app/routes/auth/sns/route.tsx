import Box from '@/components/layouts/box'
import Flex from '@/components/layouts/flex'
import SnsGoogle from '@/routes/auth/sns/components/sns.google'
import SnsKakao from '@/routes/auth/sns/components/sns.kakao'

export default function AuthSns() {
  return (
    <Box>
      <Flex className={'justify-start gap-2'}>
        <SnsGoogle />
        <SnsKakao />
      </Flex>
    </Box>
  )
}
