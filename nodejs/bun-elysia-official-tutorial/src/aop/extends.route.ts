import { Elysia, t } from 'elysia'

const extendsRoute = new Elysia()

extendsRoute
  .decorate(
    'logger',
    new (class {
      log(msg: string) {
        console.log(msg)
      }
    })(),
  )
  .state('count', 0)
  .derive({ as: 'local' }, () => ({ bye: 'bye bye~' }))
  .resolve(() => ({ boooo: 'BBBOO' }))

  .guard({
    schema: 'standalone',
    body: t.Object({
      age: t.Optional(t.Number()),
    }),
  })

  //  +-----------------------------------------------------------------------------------------+
  //  |                                                                                         |
  .macro('auth', {
    cookie: t.Object({
      session: t.Optional(t.String()),
    }),
    beforeHandle({ cookie: { session }, status }) {
      // if (!session.value) return status(401)                                                |
      if (!session.value) console.log('no coooookie')
    }, //                                                                                      |
  }) //                                                                                        |
  //                                                                                           |
  .get(
    '/extend/log',
    ({ logger }) => {
      logger.log('loggggg')
      return 'hello log'
    },
    //                                                                                         |
    { auth: true }, // <-----------------------------------------------------------------------+
  )
  .post(
    '/extend/store',
    ({ store, bye, boooo }) => {
      store.count++

      console.log(store.count, bye, boooo)

      return 'hello log'
    },
    {
      body: t.Object({
        name: t.String(),
      }),
    },
  )

export { extendsRoute }
