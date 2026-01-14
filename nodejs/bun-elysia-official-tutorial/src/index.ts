import { Elysia, t } from 'elysia'
import { env } from '@yolk-oss/elysia-env'
import openapi from '@elysiajs/openapi'

const app = new Elysia()

app
  .use(
    env({
      // APP_PORT: t.Number({default: 3000})
    }),
  )
  // .use(html())
  .use(openapi()) // http://localhost:8081/openapi

app
  .get('/', () => 'Hello Elysia')
  .get('/static/*', () => 'Hello Static')
  .get('/dynamic/:slug', ({ params: { slug } }) => `Hello ${slug}`)
  .get(
    '/dynamic/slug/:optional?',
    ({ params: { optional } }) => `Hello ${optional}`,
  )
  .get('/dynamic/wild/*', () => `Hello anything`)

  .get('/tutorial/ctx', (context) => {
    // context used as simple REQ on Express
    return {
      path: context.path,
      headers: context.headers,
    }
  })
  .get('/tutorial/status', (context) => {
    context.set.headers['Content-Type'] = 'text/html'

    return context.status(
      202,
      `<!doctype html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>Document</title>
      </head>
      <body>
        On this status 202 <br / > <a href="/tutorial/redirect">go other</a>
      </body>
      </html>`,
    )
  })
  .get('/tutorial/redirect', (context) => {
    return context.redirect('/tutorial/status')
  })
  .get('/tutorial/swg-dt', () => ({ hello: 'world' }), {
    detail: { hide: false, summary: 'WOW', description: 'let me see' },
  })
  .get('/tutorial/swg-hd', () => 'working but hide from swagger', {
    detail: { hide: true },
  })
  .post(
    '/tutorial/validate',
    (ctx) => {
      return { something: 'ok' }
    },
    {
      body: t.Object({
        name: t.String({ error: 'oops' }),
        age: t.Optional(t.Number()),
      }),
    },
  )

app.listen({ hostname: '0.0.0.0', port: process.env.APP_PORT || 3000 })

console.log(
  `🦊 Elysia is running at ${app.server?.hostname}:${app.server?.port}`,
)
