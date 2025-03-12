import { HTMLAttributes } from 'react'

export default function Fieldset({
  children,
  legend,
  className,
  disabled = false,
}: HTMLAttributes<HTMLFieldSetElement> & {
  legend?: string
  disabled?: boolean
}) {
  return (
    <fieldset
      className={`fieldset w-xs bg-base-100 border border-base-300 p-4 rounded-box ${className}`}
      disabled={disabled}
    >
      {legend ? <legend className="fieldset-legend">{legend}</legend> : <></>}
      {children}
    </fieldset>
  )
}
