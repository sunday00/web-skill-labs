export enum CompetitionStatus {
  HIDDEN,
  ACTIVE,
  CANCELED,
}

export enum CompetitionType {
  NINE,
}

export type Competition = {
  id: number
  title: string
  content: string
  userId: string
  competitionType: CompetitionType
  place: string
  price: number
  maxAttend?: number
  applyStart: string
  applyEnd: string
  competeAt: string
  status: CompetitionStatus
  createdAt: string
  updatedAt: string
}
