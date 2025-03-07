import { useContext, useEffect, useState } from 'react'
import { GlobalContext } from '@/providers/global.context.provider'

export type ToastProps = {
  status: 'success' | 'error' | 'info' | 'warning'
}

export default function Toast({
  status,
  addOffToasts,
}: ToastProps & {
  addOffToasts: () => void
}) {
  const [show, setShow] = useState(true)

  const global = useContext(GlobalContext)

  useEffect(() => {
    console.log(global.toasts)
    const st = setTimeout(() => {
      setShow(false)
      addOffToasts()
      return () => clearTimeout(st)
    }, 5000)
  })

  return (
    <>
      {show ? (
        <div className={`alert alert-${status}`}>
          <span>Message sent successfully.</span>
        </div>
      ) : (
        <></>
      )}
    </>
  )
}
