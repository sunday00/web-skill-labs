export enum ArticleType {
  APP_COMMUNITY,
  APP_NOTICE,
}

export enum ArticleCategory {
  USER_NORMAL,
}

export enum ArticleStatus {
  ACTIVE,
  HIDDEN,
}

export type Article = {
  id: number
  type: ArticleType
  category: ArticleCategory
  title: string
  content: string
  userId: string
  status: ArticleStatus
  createdAt: string
  updatedAt: string
}
