import fastify, { type FastifyError, type FastifyInstance, type FastifyServerOptions, } from 'fastify'
import swagger from '@fastify/swagger'

/**
 * validator on fastify = ajv
 * so, details about fastify validator, see ajv
 *
 * https://www.npmjs.com/package/ajv
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

  app.get('/', (req, res) => {
    return { hello: 'world' }
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
