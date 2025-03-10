import { HTMLAttributes } from 'react'

export default function Flex({ children, className }: HTMLAttributes<HTMLDivElement>) {
  const hasJustify = className?.includes('justify')
  return (
    <div className={`flex ${hasJustify ? '' : 'justify-between'} ${className}`}>{children}</div>
  )
}
