import { ReactNode } from 'react'

export const Flex = ({
  children,
  h,
  w = 'dvw',
  justify,
  align,
  gap,
}: {
  children: ReactNode
  w?: string
  h?: string
  justify?: 'center' | 'start' | 'end' | 'space-between' | 'space-around'
  align?: 'center' | 'start' | 'end'
  gap?: number
}) => {
  let cssClass = `flex justify-${justify} items-${align} `
  if (w) cssClass += ` w-${w}`
  if (h) cssClass += ` h-${h}`
  if (gap) cssClass += ` gap-${gap}`

  return <div className={cssClass}>{children}</div>
}
