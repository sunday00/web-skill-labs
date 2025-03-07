import { ReactNode } from 'react'

export default function Box({
  children,
  gap = 4,
  className,
}: {
  children: ReactNode
  gap?: number
  className?: string
}) {
  return <div className={`flex flex-col gap-${gap} ${className}`}>{children}</div>
}
