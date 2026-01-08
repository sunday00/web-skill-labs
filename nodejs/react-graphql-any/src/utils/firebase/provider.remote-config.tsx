import { type FC, type ReactNode, useEffect, useState } from 'react'
import { remoteConfig } from '@/utils/firebase/remote-config.ts'
import { fetchAndActivate } from 'firebase/remote-config'
import RemoteConfigContext from '@/utils/firebase/context.remote-config.ts'

const RemoteConfigProvider: FC<{ children: ReactNode }> = ({ children }) => {
  const [isReady, setIsReady] = useState(false)

  useEffect(() => {
    remoteConfig.defaultConfig = {}

    fetchAndActivate(remoteConfig)
      .then(() => setIsReady(true))
      .catch((err) => console.error('RC Fetch Error', err))
  }, [])

  return (
    <RemoteConfigContext.Provider value={{ isReady }}>
      {children}
    </RemoteConfigContext.Provider>
  )
}

export { RemoteConfigProvider }
