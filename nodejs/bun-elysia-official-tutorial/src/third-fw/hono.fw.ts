import { Hono } from 'hono'

const hono = new Hono().get('/aaa', (ctx) => {
  return ctx.html(`<body>Hello hono</body>`)
})

export { hono }
