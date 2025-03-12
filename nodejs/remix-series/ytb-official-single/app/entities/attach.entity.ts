export enum FileStatus {
  HIDDEN,
  SHOW,
}

export type Attach = {
  id: number
  originalName: string
  storedName: string
  path: string
  sub: object
  status: FileStatus
  userId: string
  createdAt: string
}
