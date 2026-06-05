export type Status = 'ACTIVATE' | 'INACTIVATE'

export type User = {
  id: string
  nickname: string
  snsId: string
  status: Status
}

export type Bet = {
  id: string
  category: string
  date: string
  price: number
  user: {
    nickname: string
    id: string
  }
}

export type DeleteResult = {
  deletedCount: number
}
