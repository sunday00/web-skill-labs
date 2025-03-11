import { HTMLAttributes, MouseEvent, ReactNode, useEffect, useState } from 'react'
import { FaCaretDown } from 'react-icons/fa6'
import Option, { SelectOption } from '@/components/form/select.option'
import { useSelectHelper } from '@/hooks/select.helper'

export default function Select({
  name,
  className,
  optionClassName,
  options,
  defaultValue,
  w = 'fit',
}: HTMLAttributes<HTMLDivElement> & {
  name: string
  optionClassName?: string
  options: SelectOption[]
  defaultValue?: SelectOption
  w?: string
}) {
  if (defaultValue) defaultValue.show = defaultValue.show ?? defaultValue.value.toString()
  else options[0].show = options[0].show ?? options[0].value.toString()

  const [selected, setSelected] = useState<SelectOption>(defaultValue ?? options[0])
  const [open, setOpen] = useState<boolean>(false)

  const wStyle = { width: '100%', maxWidth: '100%' }
  if (w === 'fit') {
    wStyle.width = `${Math.max(...options.map((o) => o.show?.length ?? 0)) * 0.65}em`
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
    <div className="select-input custom-select-basic flex flex-col relative w-full">
      <button
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

      {open ? (
        <ul className="select-option-list w-full absolute top-14 z-10">
          {optionLists as ReactNode}
        </ul>
      ) : (
        <></>
      )}
    </div>
  )
}
