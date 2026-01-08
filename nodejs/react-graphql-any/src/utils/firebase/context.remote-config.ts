import { createContext } from 'react'

export default createContext<{ isReady: boolean } | null>(null)
