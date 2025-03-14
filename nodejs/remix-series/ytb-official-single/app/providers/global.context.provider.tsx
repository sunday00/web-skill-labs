import { createContext, ReactNode, useEffect, useState } from 'react'

const initialState: {
  // eslint-disable-next-line
  update: (data: any) => void
  auth: {
    isLogin: boolean
    // user: User | null
  }
} = {
  // eslint-disable-next-line
  update: (data: any) => {},
  auth: {
    isLogin: false,
    // user: null,
  },
}

export const GlobalContext = createContext(initialState)

export const Providers = ({ children }: { children: ReactNode }) => {
  const [state, setState] = useState({
    ...initialState,
    update,
  })
  const [isMount, setIsMount] = useState(false)
  useEffect(() => {
    if (!isMount) {
      setIsMount(true)
    }
  }, [isMount, state])

  if (!isMount) {
    return null
  }

  // eslint-disable-next-line
  function update(data: any) {
    setState(Object.assign({}, state, data))
  }

  return <GlobalContext.Provider value={state}>{children}</GlobalContext.Provider>
}
