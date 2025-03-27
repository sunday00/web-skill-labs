import { InputHTMLAttributes, ReactNode } from 'react'

export type InputAdditional = {
  description?: string
  errorMessage?: string
}

export const InputAdditional = ({ description, errorMessage }: InputAdditional) => {
  return (
    <>
      {description ? <p className={'label-text-alt mt-1 ml-1'}>{description}</p> : null}
      <p className="label-text-alt mt-1 ml-1 text-error">{errorMessage}</p>
    </>
  )
}

export default function Input<T>({
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
}: InputHTMLAttributes<T> &
  InputAdditional & {
    label?: string
    icon?: ReactNode
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

      <InputAdditional description={description} errorMessage={errorMessage} />
    </div>
  )
}
