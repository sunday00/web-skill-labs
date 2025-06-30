type Props = { value: string; focused: boolean; edited: boolean }

type AllOptional = Partial<Props>

type SomeOptional = {
  value?: string
  focused: boolean
  ended?: boolean
  id: string
}
type AllRequired = Required<AllOptional>

type RealPartial = Pick<Props, 'value' | 'focused'>
const rp: RealPartial = {
  value: 'vv',
  focused: false,
}

type ValueOnly = Omit<Props, 'edited' | 'focused'>
