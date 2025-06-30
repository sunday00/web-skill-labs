export function ensureExhaustive(...args: number[]) {}

const HOURS_IN_DAY = 24
// ✅
if (HOURS_IN_DAY !== 24) ensureExhaustive(HOURS_IN_DAY)

// Outside of the condition, this should
// return a type error.
//❌
ensureExhaustive(HOURS_IN_DAY)

const checkExhaustive = (input: 1 | 2) => {
  switch (input) {
    case 1:
      return '!'
    case 2:
      return '!!'
    // Since all cases are handled, the default
    // branch is unreachable.
    // ✅
    default:
      ensureExhaustive(input)
  }
}

const checkNonExhaustive = (input: 1 | 2) => {
  switch (input) {
    case 1:
      return '!'
    // the case where input === 2 isn't handled,
    // so `ensureExhaustive` shouldn't be called.
    // ❌
    default:
      ensureExhaustive(input)
  }
}
