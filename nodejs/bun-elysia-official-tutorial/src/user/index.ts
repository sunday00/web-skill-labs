import { Elysia, t } from 'elysia'
import { paginateInput, userCreateInput, userService } from './user.service'

const userRoutes = ({ log }: { log: boolean }) => {
  const r = new Elysia()

  r.guard({
    // as: 'scoped',
    query: t.Object({
      hi: t.String(),
    }),
  })
    .onBeforeHandle((ctx) => {
      if (log) console.log('LOG Router before handler', ctx.request)
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

  r.get(
    '/user',
    async (ctx) => {
      const [items, total] = await userService.findManyWithCount(ctx.query)

      return { items, total, isPageList: true }
    },
    {
      query: paginateInput,
    },
  )

  return r
}

export { userRoutes }
