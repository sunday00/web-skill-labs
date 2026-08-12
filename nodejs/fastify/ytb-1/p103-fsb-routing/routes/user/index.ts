import { type FastifyPluginAsync } from 'fastify'

const users: FastifyPluginAsync = async (app, opts) => {
  app.addHook('onRequest', (req, res, done) => {
    req.log.info('hello from user hook')

    done()
  })

  app.get('/', async (req, res) => {
    return { name: 'kim' }
  })
}

export default users
