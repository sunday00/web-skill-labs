import { KeyboardEvent, MouseEvent } from 'react'

export type SelectOption = { show?: string; value: string | number }

export default function Option({
  name,
  option,
  idx,
  optionsLen,
  setOpen,
  setSelected,
}: {
  name: string
  option: SelectOption
  idx: number
  optionsLen: number
  setOpen: (open: boolean) => void
  setSelected: (option: SelectOption) => void
}) {
  option.show = option.show ?? option.value.toString()

  const isFirst = idx === 0
  const isLast = idx == optionsLen - 1

  const handleOptionKeyDown = (e: KeyboardEvent<HTMLButtonElement>) => {
    const curIdx = Number(e.currentTarget!.dataset.idx ?? 0)

    switch (e.key) {
      case 'ArrowDown': {
        const nextIdx = isLast ? 0 : curIdx + 1

        return document
          .querySelector<HTMLButtonElement>(`.${name}-select-option-${nextIdx}`)
          ?.focus()
      }
      case 'ArrowUp': {
        const nextIdx = isFirst ? optionsLen - 1 : curIdx - 1

        return document
          .querySelector<HTMLButtonElement>(`.${name}-select-option-${nextIdx}`)
          ?.focus()
      }
      case 'Tab': {
        return setOpen(false)
      }
      case 'Escape': {
        ;(document.querySelector(`.select-opener-${name}`) as HTMLButtonElement).focus()
        return setOpen(false)
      }
    }
  }

  const handleOptionClick = (_e: MouseEvent<HTMLButtonElement>) => {
    setSelected(option)
    setOpen(false)
  }

  return (
    <li className={'flex items-center bg-base-100'}>
      <button
        type={'button'}
        className={`focus:bg-darker hover:bg-darker w-full px-4 py-2 text-start select-none ${name}-select-option ${name}-select-option-${idx}`}
        tabIndex={0}
        onKeyDown={handleOptionKeyDown}
        onClick={handleOptionClick}
        data-value={option.value}
        data-idx={idx}
      >
        {option.show}
      </button>
    </li>
  )
}
