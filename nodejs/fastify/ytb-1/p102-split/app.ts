import fastify, { type FastifyInstance, type FastifyServerOptions } from 'fastify'

export async function build(opts?: FastifyServerOptions): Promise<FastifyInstance> {
  const app = opts ? fastify(opts) : fastify({ logger: true })

  app.get('/', (req, res) => {
    return { hello: 'world' }
  })

  app.get('/k', (req, res) => {
    return { hello: 'worlds' }
  })

  return app
}
