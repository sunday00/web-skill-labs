import { HTMLAttributes, LegacyRef, ReactNode } from 'react'

export default function Box({
  children,
  gap = 4,
  className = '',
  forwardRef,
}: HTMLAttributes<HTMLDivElement> & {
  children: ReactNode
  gap?: number
  forwardRef?: LegacyRef<HTMLDivElement>
}) {
  return (
    <div className={`flex flex-col gap-${gap} ${className}`} ref={forwardRef}>
      {children}
    </div>
  )
}
