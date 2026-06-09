export type Feed = {
  id: string
  content: string
  attaches: any[]
}

export type PresignedUrl = {
  cdn: string
  id: string
  sig: string
  sortKey: number
  upload: string
}
