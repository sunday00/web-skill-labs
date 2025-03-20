export enum AttendStatus {
  HIDDEN,
  APPLIED,
  SELECTED,
  PAID,
  CANCELED,
  REFUNDED,
}

export type Attend = {
  id: number
  userId: string
  competitionId: number
  code: string
  appliedAt: string
  paidAt: string
  canceledAt: string
  status: AttendStatus
  createdAt: string
  updatedAt: string
}
