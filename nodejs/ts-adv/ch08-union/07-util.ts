type R1 = Extract<'happy' | 'summer' | null | false, string>
type R2 = Extract<'onClick' | 'onSubmit' | 'event' | 'trigger', `on${string}`>

class ArticleCreateDto {
  id: number
  title: string
  description: string
  author: string
  createdAt: Date
  isActivate: boolean

  constructor(id: number, title: string, description: string, author: string, createdAt: Date) {
    this.id = id
    this.title = title
    this.description = description
    this.author = author
    this.createdAt = createdAt
    this.isActivate = true
  }
}

type R3 = Exclude<keyof ArticleCreateDto, 'author' | 'createdAt'>
type R4 = Omit<ArticleCreateDto, 'author' | 'createdAt'>
type R5 = Exclude<'onClick' | 'onSubmit' | 'event' | 'trigger', `on${string}`>

// not work
type UN<U> = U | number
type IsString<U> = UN<U> extends string ? true : false
type IS = IsString<string>

type IsString2<U> = U extends string ? true : false
type IS2 = IsString2<string | number>

type EX<A, B> = A extends B ? true : false
type R = EX<'a' | 'b' | 'c', 'a' | 'b'>

type EX2<A, B> = [A] extends [B] ? true : false
type R22 = EX2<'a' | 'b' | 'c', 'a' | 'b'>

type GenerateLink<NodeId, Criteria> = [NodeId] extends [Criteria]
  ? { from: NodeId; to: Criteria }
  : never
type Link1 = GenerateLink<'X' | 'Y' | 'Z', `${string}`>
const link1: Link1 = { from: 'X', to: 'Y' }
