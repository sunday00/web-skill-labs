export enum CommentStatus {
  ACTIVE,
  HIDDEN,
}

export type Comment = {
  id: number
  articleId: number
  parent: number
  content: string
  status: CommentStatus
  userId: string
  createdAt: string
  updatedAt: string
}
