import { ReactNode } from 'react'

export default function Box({
  children,
  gap,
  className,
}: {
  children?: ReactNode
  gap?: number
  className?: string
}) {
  let flexedClassName = 'flex flex-col ' + (className ?? '')
  if (gap) flexedClassName += ` gap-${gap}`

  return <div className={flexedClassName}>{children ?? <></>}</div>
}
