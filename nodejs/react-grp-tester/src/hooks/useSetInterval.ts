import { useEffect, useRef } from 'react'

// creating the custom useInterval hook
export function useInterval(callback: () => void, delay: number | null) {
  // Creating a ref
  const savedCallback = useRef<() => void>(() => {})

  // To remember the latest callback .
  useEffect(() => {
    savedCallback.current = callback
  }, [callback])

  // combining the setInterval and
  //clearInterval methods based on delay.
  useEffect(() => {
    function func() {
      savedCallback.current()
    }
    if (delay !== null) {
      const id = setInterval(func, delay)
      return () => clearInterval(id)
    }
  }, [delay])
}
