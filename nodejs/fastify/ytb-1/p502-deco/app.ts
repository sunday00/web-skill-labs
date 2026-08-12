import fastify, { type FastifyError, type FastifyInstance, type FastifyServerOptions, } from 'fastify'
import swagger from '@fastify/swagger'

/**
 * decorator on ts, you should declare types first
 * see type.d.ts on this folder
 */

export async function build(opts?: FastifyServerOptions): Promise<FastifyInstance> {
  const app = opts ? fastify(opts) : fastify({ logger: true })
  await app.register(swagger, {
    openapi: {
      info: { title: 'practice', version: '1.0.0' },
      servers: [
        {
          url: `http://localhost:${process.env.PORT || 3000}`,
          description: 'local',
        },
      ],
    },
  })

  app.decorate('global', { abc: 1234 }) // set decorator on global server context.
  app.decorateRequest('user', 'K') // set decorator on request context.

  app.addHook('preHandler', (req, res, done) => {
    req.user = req.user + 'K' // update decorator
    done()
  })

  app.get('/', (req, res) => {
    return { hello: 'world' }
  })

  app.get('/log', (req, res) => {
    console.log('\n\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-')
    console.log(req.user, app['global']) // using decorator
    console.log('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n')

    return { success: true }
  })

  app.setErrorHandler(async function (err: FastifyError, req, res) {
    if (err.validation) {
      res.status(err.statusCode || 400)
      return { err: err.message }
    }

    req.log.error({ err })

    res.status(err.statusCode || 500)
    return { err: err.message }
  })

  app.setNotFoundHandler(async (req, res) => {
    res.code(404)
    return 'WRONG WAY'
  })

  return app
}
