import { ReactNode } from 'react'

export default function Input({
  label,
  icon,
  type,
  name,
  autoComplete,
  placeholder,
  required = false,
  className,
  description,
  errorMessage,
}: {
  label?: string
  icon?: ReactNode
  type: string
  name: string
  autoComplete?: string
  placeholder?: string
  required?: boolean
  className?: string
  description?: string
  errorMessage?: string
}) {
  return (
    <div>
      <label className="input input-bordered flex items-center gap-2">
        {icon ? icon : <></>}
        {label ? label : ''}
        <input
          type={type}
          name={name}
          required={required}
          autoComplete={autoComplete}
          placeholder={placeholder}
          className={`input grow ${className}`}
          id={`input-${name}`}
          style={{ border: 'none' }}
        />
      </label>
      {description ? <p className={'label-text-alt mt-1 ml-1'}>{description}</p> : null}
      <p className="label-text-alt mt-1 ml-1 text-error">{errorMessage}</p>
    </div>
  )
}
