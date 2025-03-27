import Box from '@/components/layouts/box'
import { Link } from '@remix-run/react'
import Title from '@/components/texts/title'
import { CommonError } from '@/common/common.entity'

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

export const CommonUncaughtMessageShow = ({ error }: { error: CommonError }) => {
  return (
    <Box className={'flex items-center justify-center h-dvh w-full'}>
      <div>
        <Title as={2} text={`${error.statusCode} - ${error.errorData.error}`} />
        <p className={'flex justify-end mt-4'}>
          <Link to={'/'} className={'btn btn-outline'}>
            홈으로
          </Link>
        </p>
      </div>
    </Box>
  )
}
