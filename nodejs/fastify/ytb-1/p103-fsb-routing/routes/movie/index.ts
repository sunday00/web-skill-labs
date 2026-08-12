import { type FastifyPluginAsync } from 'fastify'

export const autoPrefix = '/movieee'

const movies: FastifyPluginAsync = async (app, opts) => {
  app.addHook('onRequest', (req, res, done) => {
    req.log.info('hello from movie hook')

    done()
  })

  app.get('/', async (req, res) => {
    return { title: 'oldBoy' }
  })
}

export default movies
