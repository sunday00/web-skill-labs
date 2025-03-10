import {
  HTMLAttributes,
  KeyboardEvent,
  MouseEvent,
  ReactNode,
  useEffect,
  useRef,
  useState,
} from 'react'
import { FaCaretDown } from 'react-icons/fa'

export type SelectOption = { show?: string; value: string | number }

const handleMouse = (
  el: HTMLDetailsElement | undefined | null,
  name: string,
  setCurrentFocus: (focus: number) => void,
) => {
  if (!el) return

  setCurrentFocus(0)

  if (!el.open) {
    el.open = true

    // TODO: check default value
    ;(document.querySelector(`.${name}-option-0`) as HTMLInputElement).focus()
  } else {
    el.open = false
  }
}

const handleArrowDown = (
  el: HTMLDetailsElement,
  name: string,
  currentFocus: number,
  setCurrentFocus: (focus: number) => void,
  options: SelectOption[],
) => {
  if (!el.open) {
    el.open = true
    ;(document.querySelector(`.${name}-option-0`) as HTMLInputElement).focus()
    return
  }

  if (el.open) {
    const newCurrentIndex = currentFocus >= options.length - 1 ? 0 : currentFocus + 1
    ;(document.querySelector(`.${name}-option-${newCurrentIndex}`) as HTMLInputElement).focus()
    setCurrentFocus(newCurrentIndex)
  }
}

const handleArrowUp = (
  el: HTMLDetailsElement,
  name: string,
  currentFocus: number,
  setCurrentFocus: (focus: number) => void,
  options: SelectOption[],
) => {
  if (!el.open) {
    return
  }

  const newCurrentIndex = currentFocus <= 0 ? options.length - 1 : currentFocus - 1
  ;(document.querySelector(`.${name}-option-${newCurrentIndex}`) as HTMLInputElement).focus()
  setCurrentFocus(newCurrentIndex)
}

const handleEscape = (
  el: HTMLDetailsElement,
  name: string,
  setCurrentFocus: (focus: number) => void,
) => {
  setCurrentFocus(0)
  el.open = false
  ;(document.querySelector(`.${name}-select`) as HTMLButtonElement).focus()
}

const handleTab = (
  el: HTMLDetailsElement,
  name: string,
  setCurrentFocus: (focus: number) => void,
) => {
  handleEscape(el, name, setCurrentFocus)
}

const handleBlur = (
  e: unknown,
  name: string,
  el: HTMLDetailsElement,
  setSelected: (select: SelectOption) => void,
) => {
  ;(e as MouseEvent<HTMLElement>).preventDefault()

  const target = (e as MouseEvent<HTMLInputElement>).target as HTMLInputElement

  if (target.classList?.contains(`${name}-option`)) {
    setSelected({ show: target.value, value: target.dataset.value! })
  }

  if (!target.classList.contains(`${name}-select`) && el.open) {
    ;(document.querySelector(`.${name}-select`) as HTMLButtonElement).focus()
    el.open = false
  }
}

const Option = ({ name, option, idx }: { name: string; option: SelectOption; idx: number }) => {
  option.show = option.show ?? option.value.toString()

  return (
    <input
      type={'button'}
      value={option.show}
      className={`input w-full ${name}-option ${name}-option-${idx} text-start focus:outline-none focus:bg-darker hover:bg-darker cursor-pointer`}
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
  w?: string
}) {
  if (defaultValue) defaultValue.show = defaultValue.show ?? defaultValue.value.toString()
  else options[0].show = options[0].show ?? options[0].value.toString()

  const wrapRef = useRef<HTMLDetailsElement>(null)

  const [currentFocus, setCurrentFocus] = useState(0)
  const [selected, setSelected] = useState<SelectOption>(defaultValue ?? options[0])

  const handleActive = (e: MouseEvent<HTMLElement>) => {
    e.preventDefault()
    e.stopPropagation()

    console.log(e.target as unknown as HTMLOrSVGElement)

    handleMouse(wrapRef?.current, name, setCurrentFocus)
  }

  const handleKeyDown = (e: KeyboardEvent<HTMLInputElement>) => {
    if (!wrapRef?.current) return

    switch (e.key) {
      case 'ArrowDown':
        return handleArrowDown(wrapRef.current, name, currentFocus, setCurrentFocus, options)
      case 'ArrowUp':
        return handleArrowUp(wrapRef.current, name, currentFocus, setCurrentFocus, options)
      case 'Escape':
        return handleEscape(wrapRef.current, name, setCurrentFocus)
      case 'Tab':
        return handleTab(wrapRef.current, name, setCurrentFocus)
      default:
        return
    }
  }

  const wStyle = { width: '100%', maxWidth: '100%' }
  if (w === 'fit') {
    wStyle.width = `${Math.max(...options.map((o) => o.show?.length ?? 0)) * 0.65}em`
  }

  useEffect(() => {
    if (!wrapRef.current) return
    document.querySelector('html')?.addEventListener('click', (e: unknown) => {
      handleBlur(e, name, wrapRef.current as HTMLDetailsElement, setSelected)
    })
  }, [name])

  const optionLists = options.map((option: SelectOption, idx) => {
    return <Option name={name} key={option.value} option={option} idx={idx} />
  })

  return (
    <details className={`dropdown`} ref={wrapRef} style={{ ...wStyle }}>
      <summary
        tabIndex={0}
        role="button"
        className={`input input-bordered flex justify-between items-center ${name}-select`}
        onClick={handleActive}
        onKeyDown={handleKeyDown}
      >
        <span
          role={'button'}
          className={`${name}-select`}
          style={{ userSelect: 'none', msUserSelect: 'none' }}
        >
          {selected.show}
        </span>
        <FaCaretDown />
      </summary>
      <div
        className="menu dropdown-content bg-base-100 shadowbg-base-100 rounded-b-lg z-[1] w-full p-2 shadow"
        role="button"
        tabIndex={0}
        onKeyDown={handleKeyDown}
      >
        {optionLists as ReactNode}
      </div>
    </details>
  )
}
