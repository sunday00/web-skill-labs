import fastify, {
  type FastifyError,
  type FastifyInstance,
  type FastifyServerOptions,
} from 'fastify'
import swagger from '@fastify/swagger'

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

  // ---------------------------------On Bootstrap Layer--------------------------------------------
  app.addHook('onRoute', (opt) => {
    //
    // console.log(opt)
    // if (opt.url === '/log') {
    //   opt.onRequest = async (req, res) => {
    //     req.log.info('addddd') // trigger after hook onRequest action. not overwrite.
    //   }
    // }

    if (opt.config && 'hasBig' in opt.config && opt.config.hasBig) {
      opt.onRequest = async (req, res) => {
        req.log.info('addddd by opt.config on route option.') // trigger after hook onRequest action. not overwrite.
      }
    }
  })

  // ---------------------------------OnRequest Layer-----------------------------------------------
  app.addHook('onRequest', async (req, res) => {
    req.log.warn(JSON.stringify({ url: req.url, message: 'INCOMING!!' }))
  })

  // ---------------------------------Routes and Controller layer-----------------------------------
  app.get('/', (req, res) => {
    return { hello: 'world' }
  })

  app.get('/log', (req, res) => {
    return { success: true }
  })

  app.get(
    '/log2',
    {
      config: {
        hasBig: true,
      },
    },
    (req, res) => {
      return { success: true }
    },
  )

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
