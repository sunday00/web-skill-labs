import { useContext } from 'react'
import { ToastContext } from '@/providers/global.toast.provider'

export const useToast = () => {
  const context = useContext(ToastContext)
  if (!context) {
    throw new Error('useAlert must be used within a AlertsContext')
  }
  return context
}
