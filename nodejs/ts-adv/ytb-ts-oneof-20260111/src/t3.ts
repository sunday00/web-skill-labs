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

type Test = MergeTypes<MessageTypes>

type OnlyFirst<F, S> = F & { [Key in keyof Omit<S, keyof F>]?: never }
type SimpleOneOf<F, S> = OnlyFirst<F, S> | OnlyFirst<S, F>

const message: Test = {
  id: '',
  href: '',
  text: '',
  src: '',
  timestamp: 0,
}
