type BaseMessage = { id: string; timestamp: number }
type TextMessage = BaseMessage & { text: string; href?: never }
type ImgMessage = BaseMessage & { src: string }
type UrlMessage = BaseMessage & { href: string; text?: never }

type Message = TextMessage | ImgMessage | UrlMessage

const message: Message = {
  id: '',
  href: '',
  text: '',
  timestamp: 0,
}
