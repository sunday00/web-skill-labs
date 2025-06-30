type InputA = { name: string; id: number }
type InputB = { name: string; id: string }

type InputC = InputA & InputB
const _c: InputC = {
  name: 'anv',
  // id: 1, // id is never. number & string type is never.
  id: undefined as never,
}

// So, combining overlapping attr, be careful.
type InputAA = { name: string }
type InputBB = { name: string; id: string }
interface InputD extends InputAA, InputBB {}

const d: InputD = {
  name: 'dev',
  id: '1',
}

// extends interface is faster than union type. but can't functional type.
