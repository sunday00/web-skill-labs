type Action = 'Get' | 'Post'
type Entity = 'product' | 'order'

type Attr = `${Lowercase<Action>}${Capitalize<Entity>}`
type Service = Record<Attr, Function>

const httpService = {
  getProduct: () => {},
  getOrder: () => {},
  postOrder: () => {},
  postProduct: () => {},
} satisfies Service
