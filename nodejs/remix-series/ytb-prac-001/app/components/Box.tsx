import { ReactNode } from 'react'

export const Box = ({
  children,
  gap = 4,
  className,
}: {
  children?: ReactNode
  gap?: number
  className?: string
}) => {
  const flexedClassName = `flex flex-col gap-${gap} ${className ?? ''}`

  return <div className={flexedClassName}>{children ?? <></>}</div>
}
