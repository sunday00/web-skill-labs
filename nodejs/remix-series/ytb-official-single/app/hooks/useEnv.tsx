import { useEffect, useState } from 'react'

export const useEnv = () => {
  const [env, setEnv] = useState<{ [k: string]: unknown }>({})

  useEffect(() => {
    if (window && window.env) {
      setEnv(window.env)
    }
  }, [])

  return env
}
