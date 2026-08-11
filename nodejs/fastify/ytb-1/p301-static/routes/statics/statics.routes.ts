import { type FastifyPluginAsync } from 'fastify'

const spa: FastifyPluginAsync = async (app, opts) => {
  app.get('/*', async (req, res) => {
    return res.sendFile('index.html')
  })
}

export default spa
