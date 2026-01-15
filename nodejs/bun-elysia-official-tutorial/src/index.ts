import { Elysia, t } from 'elysia'
import { env } from '@yolk-oss/elysia-env'
import openapi from '@elysiajs/openapi'
import { cookiesRoutes } from './aop/cookie.route'
import { extendsRoute } from './aop/extends.route'
import { hono } from './third-fw/hono.fw'
import { h3 } from './third-fw/h3.fw'
import { migrateDB } from './configs/db/drizzle.db.config'

class CustomErrrrr extends Error {
  status = 444

  public get statusCode() {
    return this.status
  }
}

const app = new Elysia()

app
  .use(
    env({
      // APP_PORT: t.Number({default: 3000})
    }),
  )
  // .use(html())
  .use(openapi(/* can insert type generator... */)) // http://localhost:8081/openapi

app
  .error({
    // register custom error for search by code ------------------------------------------------+
    custom1: CustomErrrrr, //                                                                   |
  }) //                                                                                         |
  .onError((err) => {
    if (err.code === 'NOT_FOUND') return err.status(404, '...;;') //            |
    //    +-------------------------------------------------------------------------------------+
    //    |
    //    \/
    if (err.code === 'custom1')
      return err.status(err.error.statusCode, 'custom.... eerrorroror')

    return err.status(400, `too bad... ${err.error}`)
  })

const userRoutes = ({ log }: { log: boolean }) => {
  const r = new Elysia()

  return r
    .guard({
      // as: 'scoped',
      query: t.Object({
        hi: t.String(),
      }),
    })
    .onBeforeHandle((ctx) => {
      if (log) console.log(ctx.request)
    })
    .onAfterHandle({ as: 'scoped' }, (ctx) => {
      if (log) console.log('scoped hook can propagate to parent also')
    })
}

app.mount('/hono', hono.fetch).mount('/h3', h3.fetch)

const PAA = app // this is for eden type
  .use(userRoutes({ log: true }))
  .use(cookiesRoutes())
  .use(extendsRoute)
  .onStart(async () => {
    // check dev
    await migrateDB()
  })
  .onBeforeHandle((ctx) => {
    console.log('this is interceptor before handle')
  })
  .onAfterHandle((ctx) => {
    console.log('this is interceptor after handle')
  })
  .onAfterResponse((ctx) => {
    console.log('this is interceptor after response')
  })
  .guard({
    beforeHandle: [
      (ctx) => {
        // if (!ctx.query.name) return ctx.status(401)
      },
      (ctx) => {
        console.log(ctx.query.name)
      },
    ],
    afterResponse({ responseValue }) {
      console.log(responseValue)
    },
  })
  .get('/', () => 'Hello Elysia')
  .get('/pm', async () => {
    return Promise.resolve(1)
  })
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
      beforeHandle(ctx) {
        console.log(ctx.query.step)
      },
      body: t.Object({
        name: t.String({ error: 'oops' }),
        age: t.Optional(t.Number()),
      }),
      query: t.Object({
        step: t.Optional(t.Number()),
      }),
    },
  )
  .get('/tutorial/errr', () => {
    throw new CustomErrrrr()
  })

// real test
// something like easy curl
app.fetch(new Request('http://localhost:8081/pm'))
// .then((res: Response) => res.text()) // <----+
// .then(console.log) // <----------------------+ i don't think this is mandatory

app.listen({ hostname: '0.0.0.0', port: process.env.APP_PORT || 3000 })

export default app
export type App = typeof PAA

console.log(
  `🦊 Elysia is running at ${app.server?.hostname}:${app.server?.port}`,
)
