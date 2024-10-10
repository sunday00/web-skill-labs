type RB = { [key: string]: boolean }

// ===

type RB2 = Record<string, boolean>

const r1: RB = {
  isKorean: false,
  isAdmin: false,
}
const r2: RB2 = {
  isMarble: true,
  isColor: false,
}

type FetchState1 = Record<'success' | 'rejected' | 'loading', boolean>
type FetchState2 = { [k in 'success' | 'rejected' | 'loading']: boolean }
type FetchState3 = { success: boolean; rejected: boolean; loading: boolean }
type FetchSuccess = FetchState1['success']
