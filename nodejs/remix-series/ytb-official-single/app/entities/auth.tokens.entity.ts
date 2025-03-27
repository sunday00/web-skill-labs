export enum AuthTokenType {
  ACCESS_TOKEN,
}

export type AuthCode = {
  id: number
  type: AuthTokenType
  userId: string
  token: string
  expiredAt: string
  createdAt: string
}
