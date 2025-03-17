import { HTMLAttributes } from 'react'

export default function Button({
  children,
  className,
  type = 'button',
  variant = 'outline',
  name,
  text,
  value,
  w,
  disabled = false,
}: HTMLAttributes<HTMLButtonElement> & {
  type?: 'submit' | 'button' | 'reset' | undefined
  variant?: 'solid' | 'outline' | 'ghost' | string
  name?: string
  text?: string
  value?: string
  w?: string
  disabled?: boolean
}) {
  return (
    <button
      className={`btn btn-${variant} btn-primary ${w ? w : 'w-28'} ${className}`}
      type={type}
      name={name}
      value={value}
      disabled={disabled}
    >
      {children ? (
        <>
          {children} {text ? <span>{text}</span> : <></>}
        </>
      ) : (
        <>{text}</>
      )}
    </button>
  )
}
