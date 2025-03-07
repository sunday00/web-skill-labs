import { createContext, ReactNode, useCallback, useEffect, useState } from 'react'
import Toast, { ToastProps } from '@/components/feedbacks/toast'

const initialState: {
  // eslint-disable-next-line
  update: (data: any) => void
  auth: {
    isLogin: boolean
    // user: User | null
  }
  toasts: ToastProps[]
} = {
  // eslint-disable-next-line
  update: (data: any) => {},
  auth: {
    isLogin: false,
    // user: null,
  },
  toasts: [],
}

export const GlobalContext = createContext(initialState)

export const Providers = ({ children }: { children: ReactNode }) => {
  const [state, setState] = useState({
    ...initialState,
    update,
  })
  const [isMount, setMount] = useState(false)
  const [toasts, setToasts] = useState<ReactNode[]>([])
  const [offToasts, setOffToasts] = useState<number>(0)
  const [toastKey, setToastKey] = useState<number>(0)

  const addOffToasts = useCallback(() => {
    setOffToasts(offToasts + 1)
  }, [offToasts])

  useEffect(() => {
    if (!isMount) setMount(true)

    // TODO??
    if (state.toasts.length) {
      const t = state.toasts.pop()
      const newToasts = [...toasts]
      newToasts.push(<Toast key={toastKey} status={t!.status} addOffToasts={addOffToasts} />)

      setToastKey(toastKey + 1)
      setToasts(newToasts)
      state.update(state)
    }

    if (toastKey && !state.toasts.length && toasts.length === offToasts) {
      setToastKey(0)
      setOffToasts(0)
      setToasts([])
    }

    console.log(toasts, toastKey, offToasts)
  }, [addOffToasts, isMount, offToasts, state, toastKey, toasts])

  if (!isMount) {
    return null
  }

  // eslint-disable-next-line
  function update(data: any) {
    setState(Object.assign({}, state, data))
  }

  return (
    <GlobalContext.Provider value={state}>
      {children}
      <div className="toast toast-end">{toasts ? (toasts as ReactNode) : <></>}</div>
    </GlobalContext.Provider>
  )
}
