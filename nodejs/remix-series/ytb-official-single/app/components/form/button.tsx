import { HTMLAttributes } from 'react'

export default function Button({
  children,
  className,
  type = 'button',
  variant = 'outline',
  text,
  w,
  disabled = false,
}: HTMLAttributes<HTMLButtonElement> & {
  type?: 'submit' | 'button' | 'reset' | undefined
  variant?: 'solid' | 'outline' | 'ghost' | string
  text?: string
  w?: string
  disabled?: boolean
}) {
  return (
    <button
      className={`btn btn-${variant} btn-primary ${w ? w : 'w-28'} ${className}`}
      type={type}
      disabled={disabled}
    >
      {children ? (
        <>
          {children} <span>{text}</span>
        </>
      ) : (
        <>{text}</>
      )}
    </button>
  )
}
