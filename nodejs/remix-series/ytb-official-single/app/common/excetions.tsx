import Box from '@/components/layouts/box'
import { Link } from '@remix-run/react'
import Title from '@/components/texts/title'

export const MethodNotAllowed = () => {
  return (
    <Box className={'flex items-center justify-center h-dvh w-full'}>
      <div>
        <Title as={2} text={'405 - 잘못된 접근'} />
        <p className={'flex justify-end mt-4'}>
          <Link to={'/'} className={'btn btn-outline'}>
            홈으로
          </Link>
        </p>
      </div>
    </Box>
  )
}
