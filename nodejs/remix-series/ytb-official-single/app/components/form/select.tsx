import { HTMLAttributes, KeyboardEvent, MouseEvent, ReactNode, useRef, useState } from 'react'
import { FaCaretDown } from 'react-icons/fa'

export type SelectOption = { show?: string; value: string | number }

const Option = ({ name, option, idx }: { name: string; option: SelectOption; idx: number }) => {
  option.show = option.show ?? option.value.toString()

  return (
    <input
      type={'button'}
      value={option.show}
      className={`input input-bordered w-full ${name}-option ${name}-option-${idx} text-start focus:outline-none focus:bg-darker hover:bg-darker cursor-pointer`}
      readOnly={true}
      data-value={option.value}
      tabIndex={0}
    />
  )
}

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
  w?: 'fit' | 'full' | string
}) {
  if (defaultValue) defaultValue.show = defaultValue!.show ?? defaultValue!.value.toString()
  else options[0].show = options[0].show ?? options[0].value.toString()

  const wrapRef = useRef<HTMLDetailsElement>(null)

  const [currentFocus, setCurrentFocus] = useState(0)
  const [selected, setSelected] = useState<SelectOption>(defaultValue ?? options[0])

  const handleActive = (e: MouseEvent<HTMLElement>) => {
    e.preventDefault()

    if (!wrapRef || !wrapRef.current) return

    setCurrentFocus(0)

    if (wrapRef.current.open) {
      wrapRef.current.open = false
      return
    }

    wrapRef.current.open = true
    ;(document.querySelector(`.${name}-option-0`) as HTMLInputElement).focus()
  }

  const handleKeyDown = (e: KeyboardEvent<HTMLInputElement>) => {
    if (!wrapRef || !wrapRef.current) return

    if (e.key === 'ArrowDown') {
      if (!wrapRef.current.open) {
        wrapRef.current.open = true
        ;(document.querySelector(`.${name}-option-0`) as HTMLInputElement).focus()
        return
      }

      if (wrapRef.current.open) {
        const newCurrentIndex = currentFocus >= options.length - 1 ? 0 : currentFocus + 1
        ;(document.querySelector(`.${name}-option-${newCurrentIndex}`) as HTMLInputElement).focus()
        setCurrentFocus(newCurrentIndex)
      }
    }

    if (e.key === 'ArrowUp') {
      if (!open) {
        return
      }

      const newCurrentIndex = currentFocus <= 0 ? options.length - 1 : currentFocus - 1
      ;(document.querySelector(`.${name}-option-${newCurrentIndex}`) as HTMLInputElement).focus()
      setCurrentFocus(newCurrentIndex)
    }

    if (e.key === 'Escape') {
      setCurrentFocus(0)
      wrapRef.current.open = false
      ;(document.querySelector(`.${name}-select`) as HTMLButtonElement).focus()
    }

    if (e.key === 'Tab') {
      setCurrentFocus(0)
      wrapRef.current.open = false
      ;(document.querySelector(`.${name}-select`) as HTMLButtonElement).focus()
    }
  }

  const optionLists = options.map((option: SelectOption, idx) => {
    return <Option name={name} key={option.value} option={option} idx={idx} />
  })

  // TODO: how to blur ?

  return (
    <details className="dropdown" ref={wrapRef}>
      <summary
        tabIndex={0}
        role="button"
        className={`input input-bordered flex justify-between items-center ${name}-select`}
        onClick={handleActive}
        onKeyDown={handleKeyDown}
      >
        <span role={'button'} style={{ userSelect: 'none', msUserSelect: 'none' }}>
          {selected.show}
        </span>
        <FaCaretDown />
      </summary>
      <div
        className="menu dropdown-content bg-base-100 shadowbg-base-100 rounded-box z-[1] w-full p-2 shadow"
        role="button"
        tabIndex={0}
        onKeyDown={handleKeyDown}
      >
        {optionLists as ReactNode}
      </div>
    </details>
  )
}
