import fastify, {
  type FastifyError,
  type FastifyInstance,
  type FastifyServerOptions,
} from 'fastify'
import swagger from '@fastify/swagger'
import fastifyAutoload from '@fastify/autoload'
import path from 'node:path'

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

  app.register(fastifyAutoload, {
    dir: path.join(process.cwd(), 'p103-fsb-routing', 'routes'),
    appendAutoPrefix: true,
    routeParams: true,
    // encapsulate: false, // <---- do not false on routes load.
    //  // encapsulated files could define hooks, decorator, sub routes each domain.
    //  // disabling encapsulate makes each registered add-on-provider being merged on global.
  })

  app.register(fastifyAutoload, {
    dir: path.join(process.cwd(), 'public'),
  })

  app.get('/', (req, res) => {
    return { hello: 'world' }
  })

  app.get('/log', (req, res) => {
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
