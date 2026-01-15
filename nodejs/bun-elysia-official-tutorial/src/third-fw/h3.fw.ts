import { createApp, createRouter, defineEventHandler, useBase } from 'h3'

const h3 = createApp()

const router = createRouter()

router.get(
  '/aaa',
  defineEventHandler((event) => {
    return 'hello h3'
  }),
)

h3.use('/**', useBase('/', router.handler))

export { h3 }
