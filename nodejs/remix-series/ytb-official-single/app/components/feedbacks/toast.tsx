import { useEffect } from 'react'
import Title from '@/components/texts/title'
import Box from '@/components/layouts/box'
import Flex from '@/components/layouts/flex'
import { IoMdCloseCircle } from 'react-icons/io'
import CloseBtn from '@/components/form/close.btn'
import { ToastProps } from '@/providers/global.toast.provider'

export const Toast = ({ attr, onDismiss }: { attr: ToastProps; onDismiss: () => void }) => {
  useEffect(() => {
    if (attr.duration! > 0 && onDismiss) {
      const timer = setTimeout(() => {
        onDismiss()
      }, attr.duration! * 1000)

      return () => clearTimeout(timer)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const handleCloseClick = () => {
    onDismiss()
  }

  return (
    <div className={`alert alert-${attr.status} block`}>
      <Box gap={1}>
        <Flex>
          <Title as={4} text={attr.title} />

          <CloseBtn onClick={handleCloseClick}>
            <IoMdCloseCircle />
          </CloseBtn>
        </Flex>
        {attr.message && attr.message !== '' && <div className="divider my-0" />}
        <span>{attr.message}</span>
      </Box>
    </div>
  )
}
