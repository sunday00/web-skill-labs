import Box from '@/components/layouts/box'
import Flex from '@/components/layouts/flex'
import SnsGoogle from '@/routes/auth/sns/components/sns.google'

export default function AuthSns() {
  return (
    <Box>
      <Flex>
        <SnsGoogle />
      </Flex>
    </Box>
  )
}
