type IsString<T> = T extends string ? true : false

type IsFifty<T> = T extends 50 ? true : false
type ItIsFifty = IsFifty<50>
type ItIsNotFifty = IsFifty<49>
type ItIsNever = IsFifty<never> // It is not 50, but assigned true. Because never is bottom type, so any type can be including never. it's very special type. remember.

// making If util type function
type IF<A extends boolean, B, C> = A extends true ? B : C

type a = IF<true, string, number>
type b = IF<false, [], {}>

type DTO<T, C extends 'create' | 'update'> = C extends 'create'
  ? Omit<T, 'id' | 'updatedAt'>
  : T

type Posting = {
  id: number
  title: string
  content: string
  createdAt: Date
  updatedAt: Date
}

type CreatePostingDto = DTO<Posting, 'create'>
type UpdatingPostingDto = DTO<Posting, 'update'>
