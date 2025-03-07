import { ReactNode } from 'react'

export default function Fieldset({
  children,
  legend,
  className,
}: {
  children: ReactNode
  legend?: string
  className?: string
}) {
  return (
    <fieldset
      className={`fieldset w-xs bg-base-200 border border-base-300 p-4 rounded-box ${className}`}
    >
      {legend ? <legend className="fieldset-legend">{legend}</legend> : <></>}
      {children}
    </fieldset>
  )
}
