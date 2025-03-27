import { HTMLAttributes, MouseEvent, ReactNode, useEffect, useState } from 'react'
import { FaCaretDown } from 'react-icons/fa6'
import Option, { SelectOption } from '@/components/form/select.option'
import { useSelectHelper } from '@/hooks/select.helper'
import { InputAdditional } from '@/components/form/input'

export default function Select({
  name,
  className,
  label,
  optionClassName,
  options,
  defaultSelect,
  description,
  errorMessage,
  w = 'fit',
}: HTMLAttributes<HTMLDivElement> &
  InputAdditional & {
    name: string
    label?: ReactNode
    optionClassName?: string
    options: SelectOption[]
    defaultSelect?: SelectOption
    description?: string
    errorMessage?: string
    w?: string
  }) {
  if (defaultSelect) defaultSelect.show = defaultSelect.show ?? defaultSelect.value.toString()
  else options[0].show = options[0].show ?? options[0].value.toString()

  const [selected, setSelected] = useState<SelectOption>(defaultSelect ?? options[0])
  const [open, setOpen] = useState<boolean>(false)

  const wStyle: { width: string | number; maxWidth: string | number; flex?: number } = {
    width: '100%',
    maxWidth: '100%',
  }
  if (w === 'fit') {
    const len = Math.max(...options.map((o) => o.show?.length ?? 0))
    wStyle.width = `${len < 10 ? len * 1.2 : len < 20 ? len * 0.78 : len * 0.65}em`
  } else if (w === 'full') {
    wStyle.flex = 1
  } else if (w) {
    wStyle.width = w
  }

  useEffect(() => {
    const handler = (e: unknown) => {
      const target = (e as { target: HTMLBodyElement }).target
      if (!target!.closest(`.select-opener-${name}`) && !target!.closest(`.${name}-select-option`))
        setOpen(false)
    }

    document.body?.addEventListener('click', handler)

    return () => document.body.removeEventListener('click', handler)
  }, [name])

  const handleMenuToggle = (_e: MouseEvent<HTMLButtonElement>) => {
    setOpen(!open)
  }

  const handleKeyDown = useSelectHelper(name, setOpen)

  const optionLists = options.map((option: SelectOption, idx) => {
    return (
      <Option
        key={option.value}
        className={`${option.value === selected.value ? 'bg-primary text-primary-content' : ''} ${optionClassName}`}
        name={name}
        option={option}
        idx={idx}
        optionsLen={options.length}
        setOpen={setOpen}
        setSelected={setSelected}
      />
    )
  })

  return (
    <div
      className={`select-input custom-select-basic flex flex-col relative w-full ${className}`}
      style={{ ...wStyle }}
    >
      {label ? (
        <label htmlFor={`select-${name}`} className={'mb-2'}>
          {label}
        </label>
      ) : (
        <></>
      )}
      <button
        id={`select-${name}`}
        tabIndex={0}
        type={'button'}
        className={`select-opener select-opener-${name} btn input input-bordered flex justify-between items-center no-animation`}
        onClick={handleMenuToggle}
        onKeyDown={handleKeyDown}
      >
        <span>{selected.show}</span>
        <FaCaretDown />
      </button>

      <input type="hidden" name={name} value={selected.value} />

      <InputAdditional description={description} errorMessage={errorMessage} />

      {open ? (
        <ul
          className={`select-option-list w-full py-2 absolute bg-base-100 ${label ? 'top-22' : 'top-14'} z-10 max-h-[14rem] border-2 overflow-scroll show-scroll-bar`}
        >
          {optionLists as ReactNode}
        </ul>
      ) : (
        <></>
      )}
    </div>
  )
}
