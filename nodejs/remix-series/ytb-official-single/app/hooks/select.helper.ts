import { KeyboardEvent } from 'react'

export const useSelectHelper = (name: string, setOpen: (open: boolean) => void) => {
  const handleArrowDown = () => {
    ;(document.querySelector('html body') as HTMLBodyElement).click()

    setOpen(true)

    setTimeout(() => {
      document.querySelector<HTMLButtonElement>(`.${name}-select-option-${0}`)?.focus()
    }, 30)
  }

  const handleArrowUp = () => {
    setOpen(false)
  }

  const handleEscape = () => {
    ;(document.querySelector(`.select-opener-${name}`) as HTMLButtonElement).focus()
    setOpen(false)
  }

  return (e: KeyboardEvent<HTMLButtonElement>) => {
    switch (e.key) {
      case 'ArrowDown':
        return handleArrowDown()
      case 'ArrowUp':
        return handleArrowUp()
      case 'Escape':
        return handleEscape()
      default:
        return
    }
  }
}
