import fastify, {
  type FastifyError,
  type FastifyInstance,
  type FastifyServerOptions,
} from 'fastify'
import swagger from '@fastify/swagger'
import createError from '@fastify/error'

const KaboomErr = createError('KaboomError-01', 'ooooooopse!!!', 500)

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

  app.get('/', (req, res) => {
    return { hello: 'world' }
  })

  app.get('/k', (req, res) => {
    return { hello: 'worlds' }
  })

  app.get('/error', (req, res) => {
    // throw new Error('Fucked')
    throw new KaboomErr()
  })

  app.get('/forced-not-found', async (req, res) => {
    res.callNotFound()
  })

  app.setErrorHandler(async function (err: FastifyError, req, res) {
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
