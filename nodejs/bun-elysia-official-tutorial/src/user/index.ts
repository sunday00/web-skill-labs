import { Elysia, t } from 'elysia'
import { userCreateInput, userService } from './user.service'

const userRoutes = ({ log }: { log: boolean }) => {
  const r = new Elysia()

  r.guard({
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

  r.post(
    '/user',
    async (ctx) => {
      const r = await userService.store(ctx.body)

      return r[0].id
    },
    {
      body: userCreateInput,
    },
  )

  return r
}

export { userRoutes }
