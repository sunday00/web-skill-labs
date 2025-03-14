import { createContext, ReactNode, useState } from 'react'
import { createPortal } from 'react-dom'
import { Toast } from '@/components/feedbacks/toast'
import { generateRandomKey } from '@/utils/random'

interface ToastProviderProps {
  children: ReactNode
}

export type ToastContextType = {
  addAlert: (prop: ToastProps) => void
}

export type ToastProps = {
  id?: string
  status: 'success' | 'error' | 'info' | 'warning'
  title: string
  message?: string
  duration?: number
}

export const ToastContext = createContext<ToastContextType | null>(null)

export default function ToastProvider({ children }: ToastProviderProps) {
  const [alerts, setAlerts] = useState<ToastProps[]>([])

  const addAlert = (prop: ToastProps) => {
    const idHash = generateRandomKey()
    const id = `toast-${new Date().getTime()}-${Math.random()}-${idHash}`

    const toast: ToastProps = {
      id,
      status: prop.status,
      title: prop.title,
      message: prop.message,
      duration: prop.duration || 9999,
    }

    setAlerts((prev) => [toast, ...prev])
    return id
  }

  const dismissAlert = (id: string) => {
    setAlerts((prev) => prev.filter((alert) => alert.id !== id))
  }

  return (
    <ToastContext.Provider value={{ addAlert }}>
      {children}
      {createPortal(
        <div className="toast toast-end">
          {alerts.map((alert) => (
            <Toast
              key={alert.id}
              attr={alert}
              onDismiss={() => {
                dismissAlert(alert.id!)
              }}
            />
          ))}
        </div>,
        document.body,
      )}
    </ToastContext.Provider>
  )
}
