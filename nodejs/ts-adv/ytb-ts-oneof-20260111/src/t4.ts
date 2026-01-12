type BaseMessage = { id: string; timestamp: number }
type TextMessage = BaseMessage & { text: string }
type ImgMessage = BaseMessage & { src: string }
type UrlMessage = BaseMessage & { href: string }

type MessageTypes = [TextMessage, ImgMessage, UrlMessage]
type MergeTypes<TypesArray extends any[], Res = {}> = TypesArray extends [
  infer Head,
  ...infer Rem,
]
  ? MergeTypes<Rem, Res & Head>
  : Res

type OneOf<
  TypesArray extends any[],
  Res = never,
  AllProperties = MergeTypes<TypesArray>,
> = TypesArray extends [infer Head, ...infer Rem]
  ? OneOf<Rem, Res | OnlyFirst<Head, AllProperties>, AllProperties>
  : Res

type OnlyFirst<F, S> = F & { [Key in keyof Omit<S, keyof F>]?: never }

const message: OneOf<MessageTypes> = {
  id: '',
  // href: '',
  // text: '',
  src: '',
  timestamp: 0,
}

const message2: OneOf<MessageTypes> = {
  id: '',
  href: '',
  // text: '',
  // src: '',
  timestamp: 0,
}
