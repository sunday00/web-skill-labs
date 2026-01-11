type BaseMessage = { id: string; timestamp: number }
type TextMessage = BaseMessage & { text: string }
type ImgMessage = BaseMessage & { src: string }
type UrlMessage = BaseMessage & { href: string }

type OnlyFirst<F, S> = F & { [Key in keyof Omit<S, keyof F>]?: never }
type SimpleOneOf<F, S> = OnlyFirst<F, S> | OnlyFirst<S, F>

const message: SimpleOneOf<TextMessage, UrlMessage> = {
  id: '',
  // href: '',
  text: '',
  timestamp: 0,
}

const message2: SimpleOneOf<TextMessage, UrlMessage> = {
  id: '',
  href: '',
  // text: '',
  timestamp: 0,
}
