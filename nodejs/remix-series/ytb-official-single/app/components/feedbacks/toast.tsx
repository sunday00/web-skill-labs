import { useContext, useEffect, useRef, useState } from 'react'
import { GlobalContext } from '@/providers/global.context.provider'

export type ToastProps = {
  status: 'success' | 'error' | 'info' | 'warning'
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
      <span>Message sent successfully.</span>
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
      const st = setTimeout(
        () => {
          if (!toastWrapRef.current?.querySelectorAll('.alert').length) {
            setToasts([])
          }
        },
        dur * 1000 + 10,
      )

      return () => clearTimeout(st)
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
