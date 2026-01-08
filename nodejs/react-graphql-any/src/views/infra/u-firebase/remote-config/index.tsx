import { Spinner } from '@chakra-ui/react'
import { useRemoteConfig } from '@/utils/firebase/hook.remote-config.tsx'

const FireBaseRemoteConfig = () => {
  const { isReady, getRCValue } = useRemoteConfig()

  if (!isReady) return <Spinner />

  const v1 = getRCValue('non_g').asString()
  const v2 = getRCValue('ccc').asString()

  return <>{v2}</>
}

export default FireBaseRemoteConfig
