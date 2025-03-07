import { ReactNode } from 'react'

export default function Button({
  children,
  type = 'button',
  variant = 'outline',
  text,
  w,
  className,
}: {
  children?: ReactNode
  type?: 'submit' | 'button' | 'reset' | undefined
  variant?: 'solid' | 'outline' | 'ghost' | string
  text?: string
  w?: string
  className?: string
}) {
  return (
    <button className={`btn btn-${variant} btn-primary ${w ? w : 'w-28'} ${className}`} type={type}>
      {children ? (
        <>
          children <span>{text}</span>
        </>
      ) : (
        <>{text}</>
      )}
    </button>
  )
}
