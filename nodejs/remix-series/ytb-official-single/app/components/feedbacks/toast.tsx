import { useContext, useEffect, useRef, useState } from 'react'
import { GlobalContext } from '@/providers/global.context.provider'
import Title from '@/components/texts/title'
import Box from '@/components/layouts/box'

export type ToastProps = {
  status: 'success' | 'error' | 'info' | 'warning'
  title: string
  message?: string
  duration: number
  key: string
}

export const Toast = ({ attr }: { attr: ToastProps }) => {
  const [show, setShow] = useState(true)

  useEffect(() => {
    if (show) {
      const st = setTimeout(() => {
        setShow(false)
      }, 1000 * attr.duration)

      return () => clearTimeout(st)
    }
  }, [attr, show])

  return show ? (
    <div className={`alert alert-${attr.status}`}>
      <Box gap={1}>
        <Title as={4} text={attr.title} />
        {attr.message && attr.message !== '' && <div className="divider my-0" />}
        <span>{attr.message}</span>
      </Box>
    </div>
  ) : (
    <></>
  )
}

export const ToastsWrap = () => {
  const global = useContext(GlobalContext)
  const [toasts, setToasts] = useState(global.toasts)
  const toastWrapRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    if (global.toasts.length) {
      const toast = global.toasts.pop()
      setToasts([...toasts, toast!])
      global.update(global)
    }

    if (toasts.length) {
      const dur = Math.max(...toasts.map((t) => t.duration))
      const st = setInterval(() => {
        if (!toastWrapRef.current?.querySelectorAll('.alert').length) {
          setToasts([])
        }
      }, dur * 800)

      return () => clearInterval(st)
    }
  }, [global, toasts])

  const list = toasts.map((t) => {
    return <Toast key={`toast-${t.key}`} attr={t} />
  })

  return (
    <div className="toast toast-end" ref={toastWrapRef}>
      {list}
    </div>
  )
}
