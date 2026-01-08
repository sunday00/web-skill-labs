import { useContext } from 'react'
import { getValue } from 'firebase/remote-config'
import { remoteConfig } from '@/utils/firebase/remote-config.ts'
import RemoteConfigContext from '@/utils/firebase/context.remote-config.ts'

const useRemoteConfig = () => {
  const context = useContext(RemoteConfigContext)
  if (!context) throw new Error('useRemoteConfig must be used within Provider')

  const getRCValue = (key: string) => {
    const val = getValue(remoteConfig, key)

    return {
      asString: () => val.asString(),
      asNumber: () => val.asNumber(),
      asBoolean: () => val.asBoolean(),
    }
  }

  return { isReady: context.isReady, getRCValue }
}

export { useRemoteConfig }
