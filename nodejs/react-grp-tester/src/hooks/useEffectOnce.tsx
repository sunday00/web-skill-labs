import * as React from 'react'
import { useEffect } from 'react'

export const useEffectOnce = (cb: React.EffectCallback) => {
  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(cb, [])
}
