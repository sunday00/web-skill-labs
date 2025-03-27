import { Article as A } from '@/entities/board.entity'

export type Article = A & { userName: string }
