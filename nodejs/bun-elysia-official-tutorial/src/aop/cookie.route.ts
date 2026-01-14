import { Elysia, t } from 'elysia'

const cookiesRoutes = () => {
  const r = new Elysia({
    // cookie: { secrets: process.env.COOKIE_SECRET },
  })

  return r
    .get(
      '/cookies',
      (ctx) => {
        // visit.set(
        //   visit.value?.total
        //     ? { value: { total: visit.value.total + 1 } }
        //     : { value: { total: 1 } },
        // )

        // visit.httpOnly = true
        // visit.path = '/cookies'
        //
        // visit.value = visit.value?.total
        //   ? { total: visit.value.total + 1 }
        //   : { total: 1 }

        if (!ctx.cookie.visit.value) {
          ctx.cookie.visit.set({
            value: { total: 0 },
            path: '/',
            httpOnly: true,
            maxAge: 40,
          })
        }

        ctx.cookie.visit.value = { total: ctx.cookie.visit.value!.total + 1 }
        console.log(ctx.cookie.visit.value)
      },
      {
        cookie: t.Object({
          visit: t.Optional(
            t.Object({
              total: t.Number(),
            }),
          ),
        }),
      },
    )
    .delete('/cookies', (ctx) => {
      ctx.cookie.visit.remove()
    })
    .get(
      '/cookies/enc',
      (ctx) => {
        if (!ctx.cookie.visit.value) {
          ctx.cookie.visit.set({
            value: { total: 0 },

            path: '/',
            httpOnly: true,
            maxAge: 40,
            // secrets: process.env.COOKIE_SECRET,
          })
        }

        // console.log('?', ctx.cookie.visit.cookie.value)

        // ctx.cookie.visit.value = { total: ctx.cookie.visit.value!.total + 1 }
        ctx.cookie.visit.set({
          value: { total: ctx.cookie.visit.value!.total + 1 },
        })

        console.log(ctx.cookie.visit.value)
      },
      {
        cookie: t.Cookie(
          {
            visit: t.Optional(
              t.Object({
                total: t.Number({ error: 'this is not number' }),
              }),
            ),
          },
          { secrets: process.env.COOKIE_SECRET, sign: ['visit'] },
        ),
        transform: (ctx) => {
          // ... encrypted secret cookie will be validated then fucked just string will be income
          // so, parse JSON need.
          if (ctx.cookie.visit.value)
            ctx.cookie.visit.value = JSON.parse(
              ctx.cookie.visit.value as unknown as string,
            )
        },
      },
    )
}

export { cookiesRoutes }
