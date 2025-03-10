import { HTMLAttributes } from 'react'

export default function CloseBtn({
  children,
  className,
  onClick,
}: HTMLAttributes<HTMLButtonElement>) {
  return (
    <button className={`rounded-full p-2 hover:bg-darker ${className}`} onClick={onClick}>
      {children}
    </button>
  )
}
