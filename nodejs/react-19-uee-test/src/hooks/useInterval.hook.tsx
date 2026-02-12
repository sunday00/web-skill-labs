import { useEffect, useEffectEvent } from 'react'

export const useInterval = (
  onTick: (tick: number) => void,
  intervalTime: number = 1000,
) => {
  const onTickEvent = useEffectEvent(onTick)
  const getSec = useEffectEvent(() => intervalTime)

  useEffect(() => {
    let tick = 0
    const interval = setInterval(() => onTickEvent(++tick), getSec())

    return () => clearInterval(interval)
  }, [])
}
