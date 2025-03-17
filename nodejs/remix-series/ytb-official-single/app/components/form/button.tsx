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
  pending = false,
}: HTMLAttributes<HTMLButtonElement> & {
  type?: 'submit' | 'button' | 'reset' | undefined
  variant?: 'solid' | 'outline' | 'ghost' | string
  name?: string
  text?: string
  value?: string
  w?: string
  disabled?: boolean
  pending?: boolean
}) {
  return (
    <button
      className={`btn btn-${variant} btn-primary ${w ? w : 'w-28'} ${className}`}
      type={type}
      name={name}
      value={value}
      disabled={disabled || pending}
    >
      {pending ? (
        <span className="loading loading-spinner" />
      ) : children ? (
        <>
          {children} {text ? <span>{text}</span> : <></>}
        </>
      ) : (
        text
      )}
    </button>
  )
}
